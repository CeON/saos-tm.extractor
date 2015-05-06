(ns saos-tm.extractor.law-links
  (:require
   [saos-tm.extractor.common :refer :all]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [langlab.core.parsers :refer [lg-split-tokens-bi]]
   [langlab.core.parsers :refer :all]
   [langlab.core.multi-stemmers :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn in? [seq elm]
  (some #(= elm %) seq))

(def coords-tokens
  ["." "," ";" "Art" "art" "ust" "par" "§" "pkt" "zd" "i"
   "oraz" "lub" "z" "-" "a" "także" "lit"])

(defn not-coords-nmb? [s]
  (nil?
   (re-matches #"\d+(-\d+)?[a-z]*|[a-u]" s)))

(defn not-coord-token? [token]
  (nil?
   (in? coords-tokens token)))

(defn get-coords-tokens [first-token-index tokens]
  (first
   (indices
    #(and (not-coord-token? %) (not-coords-nmb? %))
    (drop first-token-index tokens))))

(defn find-coords-ranges [first-token-index tokens]
  (let [
        first-non-coord-token-index
          (get-coords-tokens first-token-index tokens)
        ]
    [first-token-index
     (if (nil? first-non-coord-token-index)
       (inc (count tokens))
       (+ first-token-index first-non-coord-token-index))]))

(defn get-range [coll from to]
  (take (- to from) (drop from coll)))

(defn get-range* [coll fromto]
  (get-range coll (first fromto) (second fromto)))

(defn w-zwiazku-z? [tokens]
  (or
   (and
    (= 3 (count tokens))
    (= "w" (nth tokens 0))
    (= "związku" (nth tokens 1))
    (= "z" (nth tokens 2)))
   (and
    (= 4 (count tokens))
    (= "w" (nth tokens 0))
    (= "zw" (nth tokens 1))
    (= "." (nth tokens 2))
    (= "z" (nth tokens 3)))))

(defn handle-w-zwiazku-z [tokens-and-coords]
  (for [
        i (range 0 (count tokens-and-coords))
        ]
    (if (map? (nth tokens-and-coords i))
      (nth tokens-and-coords i)
      (if
        (and
         (w-zwiazku-z? (nth tokens-and-coords i))
         (> (count tokens-and-coords) (inc i)))
        (nth tokens-and-coords (inc i))
        (nth tokens-and-coords i)))))

(def dictionary-for-acts-strict
  [[#"(?i)^\s*Konstytucji"
    {:journalNo "78" :journalEntry "483", :journalYear "1997"}]
   [#"(?i)^\s*k\.?c" {:journalNo "16" :journalEntry "93", :journalYear "1964"}]
   [#"(?i)^\s*k\.?h"
    {:journalNo "57" :journalEntry "502", :journalYear "1934"}]
   [#"(?i)^\s*k\.?k\.?s"
    {:journalNo "83" :journalEntry "930", :journalYear "1999"}]
   [#"(?i)^\s*k\.?k\.?w"
    {:journalNo "90" :journalEntry "557", :journalYear "1997"}]
   [#"(?i)^\s*k\.?k"
    {:journalNo "88" :journalEntry "553", :journalYear "1997"}]
   [#"(?i)^\s*k\.?m"
    {:journalNo "138" :journalEntry "1545", :journalYear "2001"}]
   [#"(?i)^\s*k\.?p\.?a"
    {:journalNo "30" :journalEntry "168", :journalYear "1960"}]
   [#"(?i)^\s*k\.?p\.?c"
    {:journalNo "43" :journalEntry "296", :journalYear "1964"}]
   [#"(?i)^\s*k\.?p\.?k"
    {:journalNo "89" :journalEntry "555", :journalYear "1997"}]
   [#"(?i)^\s*k\.?p\.?w"
    {:journalNo "106" :journalEntry "1148", :journalYear "2001"}]
   [#"(?i)^\s*k\.?p"
    {:journalNo "24" :journalEntry "141", :journalYear "1974"}]
   [#"(?i)^\s*k\.?r\.?o"
    {:journalNo "9" :journalEntry "59", :journalYear "2001"}]
   [#"(?i)^\s*k\.?s\.?h"
    {:journalNo "94" :journalEntry "1037", :journalYear "2000"}]
   [#"(?i)^\s*k\.?w"
    {:journalNo "12" :journalEntry "114", :journalYear "1971"}]
   [#"(?i)^\s*k\.?z"
    {:journalNo "82" :journalEntry "598", :journalYear "1933"}]
   [#"(?i)^\s*u\.?s\.?p"
    {:journalNo "98" :journalEntry "1070", :journalYear "2001"}]
   [#"(?i)^\s*ustawy o TK"
    {:journalNo "102" :journalEntry "643", :journalYear "1997"}]
   [#"(?i)^\s*ustawy o Trybunale Konstytucyjnym"
    {:journalNo "102" :journalEntry "643", :journalYear "1997"}]
   [#"(?i)^\s*ustawy o komornikach"
    {:journalNo "133" :journalEntry "882", :journalYear "1997"}]
   [#"(?i)^\s*ustawy o ochronie konkurencji"
    {:journalNo "50" :journalEntry "331", :journalYear "2007"}]])

(defn tokens-to-string [tokens]
  (let [
        txt (str/join " " tokens)
        without-unnecessary-spaces
          (replace-several txt
                           #" \." "."
                           #" ," ","
                           #" / " "/"
                           #"\( " "("
                           #" \)" ")"
                           #" ;" ";")
        ]
    without-unnecessary-spaces))

(def not-map? (complement map?))

(defn min-index [coll]
  (.indexOf coll
            (apply min coll)))

(defn regex-first-position [re s]
  (loop [m (re-matcher re s)]
    (if (.find m)
      (.start m))))

(defn are-coords? [item]
  (map? item))

(defn extract-with-global-dictionary [tokens-or-coords dictionary]
  (if (are-coords? tokens-or-coords)
    tokens-or-coords
    (let [
          txt (tokens-to-string tokens-or-coords)
          matched-indices
            (indices
             #(not-nil? (re-find % txt))
             (map #(first %) dictionary))
          positions
            (if-not (= 1 (count matched-indices))
              (map
               #(regex-first-position (first (nth dictionary %)) txt)
               matched-indices))
          min-i
            (if-not (= 1 (count matched-indices))
              (if-not (empty? positions)
                (min-index positions)))
          first-index
            (if-not (nil? min-i)
              (nth matched-indices min-i)
              (first matched-indices))
          dictionary-record
            (if (not-nil? first-index)
              (second
               (nth dictionary first-index))
              nil)
          ]
      (if (nil? dictionary-record)
        tokens-or-coords
        dictionary-record))))

(defn act-without-entry? [tokens index-of-last-nmb]
  (cond
   (nil? index-of-last-nmb)
   true
   :else
   (and
    (> (count tokens)
       (+ index-of-last-nmb 2))
    (= "r"
       (nth tokens (inc index-of-last-nmb)))
    (= "."
       (nth tokens
            (+ index-of-last-nmb 2))))))

(defn extract-when-entry-present [parts]
  (let [
        journal-nmb-part (nth parts 0)
        entry-part (nth parts 2)
        entry
          (first
           (filter #(matches? % #"\d+") entry-part))
        index-of-last-nmb
          (last
           (indices
            #(matches? % #"\d+")
            journal-nmb-part))
        journal-nmb
          (if
            (act-without-entry? journal-nmb-part index-of-last-nmb)
            "0"
            (nth journal-nmb-part index-of-last-nmb))
        ]
    (zipmap [:journalNo :journalEntry] [journal-nmb entry])))

(defn extract-journal-nmb-and-entry [tokens]
  (let [
        parts (partition-by #(= "poz" %) tokens)
        ]
    (if (= 1 (count parts))
      (zipmap [:journalNo :journalEntry] ["0" "0"])
      (extract-when-entry-present parts))))

(defn extract-year-journal-nmb-and-entry [tokens]
  (let [
        year (get-year-of-law-act (tokens-to-string tokens))
        journal-nmb-and-entry (extract-journal-nmb-and-entry tokens)
        ]
    (zipmap
     [:journalYear :journalNo :journalEntry]
     [year
      (:journalNo journal-nmb-and-entry)
      (:journalEntry journal-nmb-and-entry)])))

(defn convert-year-to-full [year]
  (if (= (count year) 4)
    year
    (if
      (>
       (parse-int
        (str (first year)))
       1)
      (str "19" year)
      (str "20" year))))

(defn extract-journal-nmb-and-entry-dots [token]
  (let [
        numbers (drop 1 (str/split token #"\."))
        year (convert-year-to-full (first numbers))
        ]
    (cond
     (= (count numbers) 3)
     (zipmap
      [:journalYear :journalNo :journalEntry]
      [year (nth numbers 1) (nth numbers 2)])
     (= (count numbers) 2)
     (zipmap
      [:journalYear :journalNo :journalEntry]
      [year 0 (nth numbers 1)])
     :else
     (zipmap
      [:journalYear :journalNo :journalEntry]
      [year 0 0]))))

(defn extract-act-coords-journal-with-dot [tokens]
  (let [
        index-of-journal-nmb-token (.indexOf tokens "Dz.U")
        token-after-journal-nmb (nth tokens (inc index-of-journal-nmb-token))
        ]
    (if (matches? token-after-journal-nmb #"(\.\d+)+")
      (extract-journal-nmb-and-entry-dots token-after-journal-nmb)
      (extract-year-journal-nmb-and-entry tokens))))

(defn stem [s]
  (let [
        morfologik-stems (pl-multi-stem-morfologik s)
        ]
    (if (empty? morfologik-stems)
      [s]
      morfologik-stems)))

(defn stems-match? [stems1 stems2]
  ((complement empty?) (intersection (set stems1) (set stems2))))

(defn tokens-match? [tokens1 tokens2]
  (let [
        stems1 (map stem tokens1)
        stems2 (map stem tokens2)
        consecutive-positions-matches
          (map #(stems-match? %1 %2) stems1 stems2)
        ]
    ((complement contains?) (set consecutive-positions-matches) false)))

(defn local-explicit-dictionary-item-matches? [item tokens]
  (let [
        dictionary-tokens-colls (:act-abbreviation item)
        matches (filter #(tokens-match? % tokens) dictionary-tokens-colls)
        ]
    ((complement empty?) matches)))

(defn check-second-and-third-token
  [first-match-index dictionary-stems dictionary-stems-count tokens]
  (cond
    (or
     (= (count tokens) 1)
     (= (count tokens) 2)
     (= (inc first-match-index) dictionary-stems-count))
    false
   :else
    (if
      (or
     (stems-match?
      (nth dictionary-stems (inc first-match-index))
      (stem (second tokens))))
     (or
      (= (+ 2 first-match-index) dictionary-stems-count)
     (stems-match?
      (nth dictionary-stems (+ 2 first-match-index))
      (stem (nth tokens 2))))
      false)))

(defn local-implicit-dictionary-item-matches? [item tokens]
  (let [
        dictionary-stems (:act-name item)
        first-match-index
          (first
           (indices
            #(stems-match? % (stem (first tokens)))
            dictionary-stems))
        dictionary-stems-count (count dictionary-stems)
        ]
    (if (nil? first-match-index)
      false
      (check-second-and-third-token
       first-match-index dictionary-stems dictionary-stems-count tokens))))

(defn extract-with-local-explicit-dictionary [tokens-or-coords dictionary]
  (if (are-coords? tokens-or-coords)
    tokens-or-coords
    (let [
          tokens-lowercase (map str/lower-case tokens-or-coords)
          matches
            (filter
             #(local-explicit-dictionary-item-matches? % tokens-lowercase)
             dictionary)
          ]
      (if (empty? matches)
        tokens-or-coords
        (:act-coords (first matches))))))

(defn extract-with-local-implicit-dictionary [tokens-or-coords dictionary]
  (if (are-coords? tokens-or-coords)
    tokens-or-coords
    (let [
          string (tokens-to-string tokens-or-coords)
          string-lowercase (str/lower-case string)
          string-lowercase-cut
            (str/replace string-lowercase #"^\s*ustaw[^\s]*\s+(o|z)?" "")
          matches
            (filter
             #(local-implicit-dictionary-item-matches?
               %
               (split-to-tokens string-lowercase-cut))
             dictionary)
          ]
      (if (empty? matches)
        tokens-or-coords
        (:act-coords (first matches))))))

(defn cut-tokens-for-act-coords [tokens]
  (let [
        string (tokens-to-string tokens)
        string-cut
          (first
           (str/split
            string
            (re-pattern
             (str "w\\s*zw\\.\\s*z|"
                  "w\\s*związku\\s*z|"
                  "[^e]\\s(U|u)staw|"
                  "\\s(R|r)ozporządz|"
                  "\\)"))))
        tokens-cut (split-to-tokens string-cut)
        ]
    (if (empty? tokens-cut)
      tokens
      tokens-cut)))

(defn extract-with-dictionaries
  [local-explicit-dictionary local-implicit-dictionary global-dictionary
   tokens-cut]
  (let [
        extract-with-local-explicit
          (if ((complement empty?) local-explicit-dictionary)
            #(extract-with-local-explicit-dictionary
              % local-explicit-dictionary)
            identity)
        extract-with-local-implicit
          (if ((complement empty?) local-implicit-dictionary)
            #(extract-with-local-implicit-dictionary
              % local-implicit-dictionary)
            identity)
        extract-with-global
          (if ((complement empty?) global-dictionary)
            #(extract-with-global-dictionary
              % global-dictionary)
            identity)
        ]
    (-> tokens-cut
        extract-with-global
        extract-with-local-explicit
        extract-with-local-implicit)))

(defn extract-act-coords-greedy
  [tokens local-explicit-dictionary local-implicit-dictionary
   global-dictionary]
  (if
    (w-zwiazku-z? tokens)
    tokens
    (let [
          tokens-cut (cut-tokens-for-act-coords tokens)
          ]
      (cond
       (some #{"Dz.U"} tokens-cut)
       (extract-act-coords-journal-with-dot tokens)
       (and (some #{"Dz"} tokens-cut) (some #{"U"} tokens-cut))
       (extract-year-journal-nmb-and-entry tokens)
       :else
       (extract-with-dictionaries
        local-explicit-dictionary local-implicit-dictionary global-dictionary
        tokens-cut)))))

(defn extract-act-coords-strict [tokens & _]
  (let [
        extracted-with-dictionary
          (extract-with-global-dictionary tokens dictionary-for-acts-strict)
        ]
    (if (map? extracted-with-dictionary)
      extracted-with-dictionary
      (cond
       (some #{"Dz.U"} tokens)
       (extract-act-coords-journal-with-dot tokens)
       (and (some #{"Dz"} tokens) (some #{"U"} tokens))
       (extract-year-journal-nmb-and-entry tokens)))))

(defn coord-to-text [token]
  (if (or (= "." token) (= "-" token))
    token
    (str " " token)))

(defn build-coords-text [tokens-range tokens]
  (str/replace
   (str/join ""
             (map
              coord-to-text
              (get-range* tokens tokens-range)))
   "- " "-"))

(defn get-interfering-art-coords-ranges [tokens]
  (map
   #(find-coords-ranges % tokens)
   (indices
    #(or (= % "art") (= % "Art") (= % "§"))
    tokens)))

(defn split-sum-of-colls-to-pairs [coll1 coll2]
    (partition 2 (concat coll1 coll2)))

(defn get-inter-coords-ranges-candidates
  [interfering-art-coords-ranges tokens]
  (split-sum-of-colls-to-pairs
   (drop 1 (flatten interfering-art-coords-ranges))
   [(count tokens)]))

(defn get-inter-coords-ranges [tokens]
  (let [
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        ]
    (filter
     #(< (first %) (second %))
     (get-inter-coords-ranges-candidates
      interfering-art-coords-ranges tokens))))

(defn get-correct-art-coords-ranges [tokens]
  (let [
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        inter-coords-ranges (get-inter-coords-ranges tokens)
        ]
    (split-sum-of-colls-to-pairs
     [(first (first interfering-art-coords-ranges))]
     (flatten inter-coords-ranges))))

(defn get-line-with-signature [s]
  (let [
        lines (str/split s (re-pattern system-newline))
        lines-with-sygn-text (filter #(.startsWith % "Sygn.") lines)
        index-of-first-line-ending-with-date
          (first
           (indices
            #(.endsWith % " r.")
            lines))
        ]
    (if (empty? lines-with-sygn-text)
      (nth lines (+ index-of-first-line-ending-with-date 1))
      (first lines-with-sygn-text))))

(def art-coords-names [:art :par :ust :pkt :zd :lit])

(defn zip-link-to-map [link]
  (let [
        art (:art link)
        act (:act link)
        ]
    (map
     #(zipmap
       [:art :act]
       [(zipmap art-coords-names %1) act])
     art)))

(defn get-data-for-orphaned-link [orphaned-link]
  (let [
        txt (tokens-to-string (:act orphaned-link))
        ]
    (map
     #(zipmap
       [:txt :art]
       [txt %])
     (:art orphaned-link))))

(defn cleanse [s]
  (when (not-nil? s)
    (replace-several s
                     #"\." ""
                     #"\s" "")))

(defn cleanse-link [link]
  (let [
        art (:art link)
        act (:act link)
        ]
    (zipmap
     [:act :art]
     [(zipmap [:journalYear :journalNo :journalEntry]
              [(cleanse (:journalYear act))
               (cleanse (:journalNo act))
               (cleanse (:journalEntry act))])
      (zipmap art-coords-names
              [(cleanse (:art art))
               (cleanse (:par art))
               (cleanse (:ust art))
               (cleanse (:pkt art))
               (cleanse (:zd art))
               (cleanse (:lit art))])])))

(defn print-art-act-texts
  [tokens correct-art-coords-ranges inter-coords-ranges]
  (let [
        art-coords-texts
          (map
           #(build-coords-text % tokens)
           correct-art-coords-ranges)
        act-coords-texts
          (map
           #(build-coords-text % tokens)
           inter-coords-ranges)
        ]
    (doall
     (map
      #(prn (str %1 " | " %2))
      art-coords-texts
      act-coords-texts))))

(defn cleanse-act-abbrevation [s]
  (str/trim
   (replace-several s
                    #"^\s*również" ""
                    #"^[^A-Za-ząćęłńóśżźĄĆĘŁŃÓŚŻŹ\.]+" ""
                    #"[^A-Za-ząćęłńóśżźĄĆĘŁŃÓŚŻŹ\\.]+$" "")))

(def explicit-local-dictionary-definition-regex
  (re-pattern
   (str
    ";\\s*dalej\\s*:?|"
    ",\\s*dalej\\s*:?|"
    "zwana\\s*dalej\\s*:?|"
    "zwanej\\s*dalej\\s*:?|"
    "\\(dalej\\s*:?")))

(defn extract-dictionary-item [parts]
  (let [
        act-abbreviation-txt (second parts)
        act-abbreviation-without-dash
          (str/replace act-abbreviation-txt #"^\s*–" "")
        act-abbreviation-cut
          (str/trim
           (first
            (str/split
             act-abbreviation-without-dash
             #":|\)|\.\s[A-ZĄĆĘŁŃÓŚŻŹ]|,|–|”\s[A-ZĄĆĘŁŃÓŚŻŹ]")))
        act-abbreviation-cleansed
          (cleanse-act-abbrevation act-abbreviation-cut)
        act-abbreviation-coll
          (if (substring? " lub " act-abbreviation-cleansed)
            (map cleanse-act-abbrevation
                 (str/split act-abbreviation-cleansed #"\slub\s"))
            [act-abbreviation-cleansed])
        act-abbreviation-coll-lowercase
          (map str/lower-case act-abbreviation-coll)
        act-abbreviation-coll-tokens
          (map split-to-tokens act-abbreviation-coll-lowercase)
        act-coords
          (extract-year-journal-nmb-and-entry
           (split-to-tokens (first parts)))
        ]
    {:act-coords act-coords
     :act-abbreviation act-abbreviation-coll-tokens}))

(def journal-regex #"[\S\s]*Dz\.\s*U\.[\S\s]*")

(defn doesnt-contain-journal? [s]
  ((complement matches?) s journal-regex))

(defn get-local-explicit-dictionary-item [s]
  (if (doesnt-contain-journal? s)
  nil
  (let [
        parts
          (str/split
           s
           explicit-local-dictionary-definition-regex)
        ]
    (if (= (count parts) 1)
      nil
      (extract-dictionary-item parts)))))

(defn get-local-implicit-dictionary-item [s]
  (if (doesnt-contain-journal?
       (first (str/split s #"w\s*zw\.\s*z|w\s*związku\s*z")))
  nil
  (let [
        act-name
          (map stem
               (split-to-tokens
                (first
                 (str/split (str/lower-case s) #"\(|\["))))
        act-coords
          (extract-year-journal-nmb-and-entry (split-to-tokens s))
        ]
    {:act-coords act-coords
     :act-name   act-name})))

(defn get-local-dictionary [act-coords-txts get-dictionary-item-fn]
  (let [
        local-dictionary-with-nils
          (map get-dictionary-item-fn act-coords-txts)
        local-dictionary
          (remove nil? local-dictionary-with-nils)
        ]
    local-dictionary))

(defn return-empty-coll [arg1 arg2] [])

(defn get-range-from-first-law-act-to-first-article-token
  [tokens first-art-index]
  (if (nil? first-art-index)
    nil
    (let [
          first-law-act-token-index
            (first
             (indices
             #(or (= "ustawa" %) (= "ustawy" %))
             (get-range tokens 0 first-art-index)))
          ]
      (if (nil? first-law-act-token-index)
        nil
        [first-law-act-token-index first-art-index]))))

(defn extract-law-links
  [s extract-act-coords-fn
   use-local-explicit-dictionary use-local-implicit-dictionary
   use-global-dictionary]
  (let [
        extract-local-explicit-dicitionary-fn
          (if use-local-explicit-dictionary
            #(get-local-dictionary % get-local-explicit-dictionary-item)
            #(return-empty-coll % nil))
        extract-local-implicit-dicitionary-fn
          (if use-local-implicit-dictionary
            #(get-local-dictionary % get-local-implicit-dictionary-item)
            #(return-empty-coll % nil))
        global-dictionary
          (if use-global-dictionary
            dictionary-for-acts-strict
            [])

        preprocessed (preprocess s)
        txt (replace-several preprocessed
                             #"art\." " art. "
                             #"ust\." " ust. "
                             #"§" " § "
                             #"pkt" " pkt "
                             #"zd\." " zd. "
                             #"poz\." " poz. ")
        tokens (split-to-tokens txt)
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        inter-coords-ranges
          (get-inter-coords-ranges tokens)
        correct-art-coords-ranges
          (get-correct-art-coords-ranges tokens)
        art-coords
          (map
           extract-art-coords
           (map #(build-coords-text % tokens) correct-art-coords-ranges))
        range-from-first-law-act-to-first-article-token
          (get-range-from-first-law-act-to-first-article-token
           tokens
           (first (first inter-coords-ranges)))
        act-coords-txts
          (map
           #(build-coords-text % tokens)
           (if (nil? range-from-first-law-act-to-first-article-token)
             inter-coords-ranges
             (concat [range-from-first-law-act-to-first-article-token]
                     inter-coords-ranges)))
        acts-txts (str/split s #"\sustaw(a|y)\s")

;;         _ (prn "=========== EXP")
;;         _ (prn (extract-local-explicit-dicitionary-fn act-coords-txts))
;;         _ (prn "=========== IMP")
;;         _ (prn (extract-local-implicit-dicitionary-fn act-coords-txts))
;;         _ (prn "===========")

        act-coords
          (handle-w-zwiazku-z
           (map
            #(extract-act-coords-fn
              %
              (extract-local-explicit-dicitionary-fn
               (concat act-coords-txts acts-txts))
              (extract-local-implicit-dicitionary-fn act-coords-txts)
              global-dictionary)
            (map
             #(get-range tokens (first %) (second %))
             inter-coords-ranges)))
        links
          (distinct
           (map #(zipmap [:art :act] [%1 %2])
                art-coords act-coords))
        extracted-links (filter #(map? (:act %)) links)
        extracted-links-maps (mapcat zip-link-to-map extracted-links)
        extracted-links-cleansed (map cleanse-link extracted-links-maps)
        orphaned-links
          (flatten
           (filter
            #(not-map? (:act %))
            links))
        ]
    (zipmap
     [:extracted-links :orphaned-links]
     [ (into [] extracted-links-cleansed)
       (into []
             (mapcat get-data-for-orphaned-link orphaned-links))])))

(defn extract-law-links-greedy
  [s
   use-local-explicit-dictionary use-local-implicit-dictionary
   use-global-dictionary]
  (extract-law-links
   s extract-act-coords-greedy
   use-local-explicit-dictionary
   use-local-implicit-dictionary
   use-global-dictionary))

(defn extract-law-links-strict [s]
  (extract-law-links
   s extract-act-coords-strict
   false false false))

; Utilities for the art part of extracted link.
; Includes converting to string and sorting.

(defn ^:private append-prefix-and-suffix-if-suffix-not-zero
  [ ^String s ^String prefix  ^String suffix ]
  (if-not (= suffix "0")
    (str s " " prefix " " suffix)
    s))

(defn convert-art-to-str [ art ]
  (-> ""
    (append-prefix-and-suffix-if-suffix-not-zero "art." (:art art))
    (append-prefix-and-suffix-if-suffix-not-zero "§" (:par art))
    (append-prefix-and-suffix-if-suffix-not-zero "ust." (:ust art))
    (append-prefix-and-suffix-if-suffix-not-zero "pkt" (:pkt art))
    (append-prefix-and-suffix-if-suffix-not-zero "zd." (:zd art))
    (append-prefix-and-suffix-if-suffix-not-zero "lit." (:lit art))
    (str/trim)))

(defn ^:private chain-compare [ res s1 s2 ]
  (if (= 0 res)
    (compare s1 s2)
    res))

(defn ^:private append-zero-if-no-letter [s]
  (if-not (re-find #"[a-z]$" s)
    (str s "0")
    s))

(defn add-spaces ^:private [n s]
  (str (apply str (repeat n " ")) s))

(defn ^:private conv-number-letter-for-comparison [ s1 s2 ]
  (let [
        s1* (append-zero-if-no-letter s1)
        s2* (append-zero-if-no-letter s2)
        n (- (.length s1*) (.length s2*))
        ]
    [ (add-spaces (- n) s1*) (add-spaces n s2*)]))

(defn ^:private chain-compare-number-letter [ res s1 s2 ]
  (if (= 0 res)
    (apply compare
      (conv-number-letter-for-comparison s1 s2))
    res))

(defn ^:private compare-arts [ art1 art2 ]
  (-> 0
    (chain-compare-number-letter (:art art1) (:art art2))
    (chain-compare-number-letter (:par art1) (:par art2))
    (chain-compare-number-letter (:ust art1) (:ust art2))
    (chain-compare-number-letter (:pkt art1) (:pkt art2))
    (chain-compare (:lit art1) (:lit art2))
    (chain-compare (:zd art1) (:zd art2))))

(defn sort-arts [ arts ]
  (sort-by identity compare-arts arts))
