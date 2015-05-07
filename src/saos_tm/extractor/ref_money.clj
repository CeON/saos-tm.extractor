(ns saos-tm.extractor.ref-money
  (:require
    [clojure.string :as str ]
    [langlab.core.parsers :as parsers]
    [langlab.core.characters :as chars]
    [automat.core :as a]))

(defn ^:private take-subseq [ i j seq ]
  (take (- j i) (drop i seq)))

(defn ^:private get-string [ token-map ]
  (if (keyword? (:token token-map))
    (:value token-map)
    (:token token-map)))

(defn ^:private is-number-token? [s]
  (some? (re-matches #"[,.0-9]+" s)))

(defn ^:private is-number-with-decade-and-decimal-sep?
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

(defn ^:private is-number-with-decimal-sep?
  ([s decimal-sep]
     (let [
            number-re
              (re-pattern
                (str
                  "^[1-9][0-9]*\\" decimal-sep  "[0-9]{1,2}?$" "|"
                  "^0\\" decimal-sep "[0-9]{1,2}?$" "|"  "[1-9][0-9]*"))
          ]
       (some? (re-matches number-re s)))))

(defn ^:private is-number? [ s decimal-sep decade-sep ]
  (let [
         has-decade-sep? (>= (.indexOf s (str decade-sep)) 0)
       ]
    (if has-decade-sep?
      (is-number-with-decade-and-decimal-sep? s decimal-sep decade-sep)
      (is-number-with-decimal-sep? s decimal-sep))))

(defn ^:private conv-to-num [ s ]
  (try
    (bigdec s)
    (catch Exception e "ERROR")))

(defn ^:private parse-number [s decimal-sep decade-sep ]
  (-> s
      (str/replace decade-sep "")
      (str/replace decimal-sep ".")
      conv-to-num))

(defn ^:private is-not-blank-token? [ ^String token]
  (not (chars/contains-whitespace-only? token)))

(defn ^:private is-not-blank-token-map? [ token-map ]
  (let [
      token (:token token-map)
    ]
  (if (string? token)
    (is-not-blank-token? token)
    true)))

(defn ^:private keywordize-numbers-in-token-map [ token-map ]
  (let [
         token (:token token-map)
       ]
    (if (and (string? token) (is-number-token? token))
      (assoc token-map :token :num :value token)
      token-map)))

(defn ^:private keywordize-line-breaks-in-token-map [ token-map ]
  (let [
        token ^String (:token token-map)
        ]
    (if (.contains token "\n")
      (assoc token-map :token :br :value token)
      token-map)))

(defn ^:private parse-to-tokens [s]
  (parsers/split* s #"(?m)\s+|\(|\)"))

(defn ^:private conv-tokens-to-plain-maps [ tokens ]
  (map
    #(hash-map :token  %1 :index %2)
    tokens (range)))

(defn ^:private conv-tokens-to-cleaned-maps [ tokens ]
  (->> tokens
    conv-tokens-to-plain-maps
    (map keywordize-line-breaks-in-token-map)
    (filter is-not-blank-token-map?)
    (map keywordize-numbers-in-token-map)))

(def ^:private money-suffix-multiply-kilo
  ["tys." "tys"])

(def ^:private money-suffix-multiply-mega
  ["mln" "mln."])

(def money-suffix-multiply
  (concat money-suffix-multiply-kilo money-suffix-multiply-mega))

(def ^:private money-suffix-currency-zl
  ["zł." "zł," "zł" "zł;" "zł?"])

(def ^:private money-suffix-currency-gr
  ["gr." "gr," "gr" "gr;" "gr?"])

(def ^:private a-money
  [ (a/+ :num )
    (a/? :br)
    (a/? (apply a/or money-suffix-multiply))
    (a/? :br)
    (apply a/or money-suffix-currency-zl)
    (a/? [ (a/? :br) :num (a/? :br) (apply a/or money-suffix-currency-gr) ])])

(def ^:private a-money-c
  (a/compile
    a-money
    { :signal :token }))

(defn ^:private conv-automat-state-to-raw-money-refs [state tokens]
  (let [
         i (:start-index state)
         j (:stream-index state)
       ]
  [ (:index (nth tokens i))
    (inc (:index (nth tokens (dec j))))
    (map get-string (take-subseq i j tokens)) ]))

(defn ^:private handle-number [ [res tokens] ]
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

(defn ^:private handle-multiply-suffix [ [res tokens] ]
  (if-not (empty? tokens)
    (cond
      (.contains money-suffix-multiply-kilo (first tokens))
        [ (* res 1000) (rest tokens) ]
      (.contains money-suffix-multiply-mega (first tokens))
        [ (* res 1000000) (rest tokens) ]
      :else
        [res tokens])
    [res tokens]))

(defn ^:private handle-currency-suffix-zl [ [res tokens] ]
  (if-not (empty? tokens)
    (if (.contains money-suffix-currency-zl (first tokens))
      [res (rest tokens)]
      ["ERROR" []])
    [res tokens]))

(defn ^:private handle-gr [ [res tokens] ]
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

(defn ^:private handle-finalize [ [res tokens] ]
  (if (empty? tokens)
    res
    "ERROR"))

(defn ^:private conv-tokens-to-value [ value-tokens ]
  (->> [ 0M value-tokens ]
      (handle-number)
      (handle-multiply-suffix)
      (handle-currency-suffix-zl)
      (handle-gr)
      (handle-finalize)))

(defn ^:private conv-tokens-range-to-str [ tokens start stop  ]
  (apply str (take-subseq start stop tokens)))

(defn ^:private extract-raw-money-refs-from-token-maps [ results tokens ]
  (let [
          state (a/greedy-find a-money-c nil tokens)
          n (:stream-index state)
       ]
    (if-not (:accepted? state)
      results
      (recur
        (conj results (conv-automat-state-to-raw-money-refs state tokens))
        (drop n tokens)))))

(defn ^:private clean-final-punct [s]
  (str/replace s #"[,.;?]$" ""))

(defn ^:private normalize-raw-money-refs [tokens [i j value-tokens]]
  {
    :amount
      (conv-tokens-to-value
        (filter is-not-blank-token? value-tokens))
    :text
      (clean-final-punct
        (conv-tokens-range-to-str tokens i j))
  })

(defn extract-money-refs [s]
  (let [
         tokens
           (parse-to-tokens s)
         money-refs-raw
           (extract-raw-money-refs-from-token-maps
             []
             (conv-tokens-to-cleaned-maps tokens))
       ]
    (into []
      (map
        (partial normalize-raw-money-refs tokens)
        money-refs-raw))))

(defn ^:private select-max-money-ref [ money-refs ]
  (let [
        amounts
          (filter number? (map :amount money-refs))
        max-amount
          (when-not (empty? amounts) (apply max amounts))
        ]
    (when max-amount
      (some #(when (= max-amount (:amount %)) %) money-refs))))

(defn extract-max-money-ref [s]
  (select-max-money-ref (extract-money-refs s)))
