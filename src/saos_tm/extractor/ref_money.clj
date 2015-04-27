(ns saos-tm.extractor.ref-money
  (:require
    [clojure.string :as str ]
    [langlab.core.parsers :as parsers]
    [langlab.core.characters :as chars]
    [automat.core :as a]))

(defn take-subseq [ i j seq ]
  (take (- j i) (drop i seq)))

(defn get-string [ token-map ]
  (if (keyword? (:token token-map))
    (:value token-map)
    (:token token-map)))

(defn is-number-token? [s]
  (some? (re-matches #"[,.0-9]+" s)))

(defn is-number-with-decade-and-decimal-sep?
  "It assumes that `s` contains both `decimal-sep` and `decade-sep`.
   E.g. 100.0 is not a valid number, because it does not contain `decade-sep`"
  [s decimal-sep decade-sep]
     (let [
            number-re
              (re-pattern
                (str "^[1-9][0-9]{0,2}(?:\\" decade-sep "[0-9]{3})*"
                     "(?:\\" decimal-sep "[0-9]{1,2})?$"))
          ]
       (some? (re-matches number-re s))))

(defn is-number-with-decimal-sep?
  ([s decimal-sep]
     (let [
            number-re
              (re-pattern
                (str
                  "^[1-9][0-9]*\\" decimal-sep  "[0-9]{1,2}?$" "|"
                  "^0\\" decimal-sep "[0-9]{1,2}?$" "|"  "[1-9][0-9]*"))
          ]
       (some? (re-matches number-re s)))))

(defn is-number? [ s decimal-sep decade-sep ]
  (let [
         has-decade-sep? (>= (.indexOf s (str decade-sep)) 0)
       ]
    (if has-decade-sep?
      (is-number-with-decade-and-decimal-sep? s decimal-sep decade-sep)
      (is-number-with-decimal-sep? s decimal-sep))))

(defn conv-to-num [ s ]
  (try
    (bigdec s)
    (catch Exception e "ERROR")))

(defn parse-number [s decimal-sep decade-sep ]
  (-> s
      (str/replace decade-sep "")
      (str/replace decimal-sep ".")
      conv-to-num))

(defn is-not-blank-token-map? [ token-map ]
  (not (chars/contains-whitespace-only? (:token token-map))))

(defn keywordize-token-map [ token-map ]
  (let [
         token (:token token-map)
       ]
    (if (is-number-token? token)
      (assoc token-map :token :num :value token)
      token-map)))

(defn parse-to-tokens [s]
  (parsers/split* s #"(?m)\s+"))

(defn conv-tokens-to-maps [ tokens ]
  (as->
    tokens ---
    (map
      #(hash-map :token  %1 :index %2)
      --- (range))
    (filter is-not-blank-token-map? ---)
    (map keywordize-token-map ---)))

(def money-suffix-multiply-kilo
  ["tys." "tys"])

(def money-suffix-multiply-mega
  ["mln" "mln."])

(def money-suffix-multiply
  (concat money-suffix-multiply-kilo money-suffix-multiply-mega))

(def money-suffix-currency-zl
  ["zł." "zł," "zł"])

(def money-suffix-currency-gr
  ["gr." "gr," "gr"])

(def a-money
  [ (a/+ :num )
    (a/? (apply a/or money-suffix-multiply))
    (apply a/or money-suffix-currency-zl)
    (a/? [ :num (apply a/or money-suffix-currency-gr) ])])

(def a-money-c
  (a/compile
    a-money
    { :signal :token }))

(defn conv-automat-state-to-raw-money-refs [state tokens]
  (let [
         i (:start-index state)
         j (:stream-index state)
       ]
  [ (:index (nth tokens i))
    (inc (:index (nth tokens (dec j))))
    (map get-string (take-subseq i j tokens)) ]))

(defn handle-number [ [res tokens] ]
  (if-not (empty? tokens)
    (let [
           [number-tokens rest-tokens]
             (split-with is-number-token? tokens)
           number-str
             (apply str number-tokens)
           number
             (cond
               (is-number? number-str "," ".") (parse-number number-str "," ".")
               (is-number? number-str "." ",") (parse-number number-str "." ",")
               :else  "ERROR")
         ]
      (if (number? number)
        [number rest-tokens]
        [number []]))
      [res tokens ]))

(defn handle-multiply-suffix [ [res tokens] ]
  (if-not (empty? tokens)
    (cond
      (.contains money-suffix-multiply-kilo (first tokens))
        [ (* res 1000) (rest tokens) ]
      (.contains money-suffix-multiply-mega (first tokens))
        [ (* res 1000000) (rest tokens) ]
      :else
        [res tokens])
    [res tokens]))

(defn handle-currency-suffix-zl [ [res tokens] ]
  (if-not (empty? tokens)
    (if (.contains money-suffix-currency-zl (first tokens))
      [res (rest tokens)]
      ["ERROR" []])
    [res tokens]))

(defn handle-gr [ [res tokens] ]
  (if-not (empty? tokens)
    (let [
           [number-tokens rest-tokens]
             (split-with is-number-token? tokens)
           gr?
             (and
               (= 1 (count rest-tokens))
               (.contains money-suffix-currency-gr (first rest-tokens)))
           number-str
             (apply str number-tokens)
           gr-amount
              (if (and gr? (re-matches #"[0-9]{1,2}" number-str))
                (try
                  (bigdec number-str)
                  (catch Exception e "ERROR"))
                "ERROR")

         ]
      (if (= gr-amount "ERROR")
        [ "ERROR" [] ]
        [ (+ res (bigdec (/ gr-amount 100))) [] ]))
    [res tokens]))

(defn handle-finalize [ [res tokens] ]
  (if (empty? tokens)
    res
    "ERROR"))

(defn conv-tokens-to-value [ value-tokens ]
  (->> [ 0M value-tokens ]
      (handle-number)
      (handle-multiply-suffix)
      (handle-currency-suffix-zl)
      (handle-gr)
      (handle-finalize)))

(defn conv-tokens-range-to-str [ tokens start stop  ]
  (apply str (take-subseq start stop tokens)))

(defn extract-raw-money-refs-from-token-maps [ results tokens ]
  (let [
          state (a/greedy-find a-money-c nil tokens)
          n (:stream-index state)
       ]
    (if-not (:accepted? state)
      results
      (recur
        (conj results (conv-automat-state-to-raw-money-refs state tokens))
        (drop n tokens)))))

(defn normalize-raw-money-refs [tokens [i j value-tokens]]
  {
    :amount (conv-tokens-to-value value-tokens)
    :text (conv-tokens-range-to-str tokens i j)
  })

(defn extract-money-refs [s]
  (let [
         tokens
           (parse-to-tokens s)
         money-refs-raw
           (extract-raw-money-refs-from-token-maps [] (conv-tokens-to-maps tokens))
       ]
    (into []
      (map
        (partial normalize-raw-money-refs tokens)
        money-refs-raw))))



