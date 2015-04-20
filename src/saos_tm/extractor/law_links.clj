(ns saos-tm.extractor.law-links
  (:require
   [saos-tm.extractor.common :refer :all]
   [clojure.string :as str ]
   [langlab.core.parsers :refer [lg-split-tokens-bi]])
  (:import java.io.File)
  (:gen-class))

(defn in? [seq elm]
  (some #(= elm %) seq))

(def coords-tokens
  ["." "," "Art" "art" "ust" "par" "§" "pkt" "zd" "i"
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
  [[#"(?i)^Konstytucji"
    {:journalNo "78" :journalEntry "483", :journalYear "1997"}]
   [#"(?i)^k\.?c" {:journalNo "16" :journalEntry "93", :journalYear "1964"}]
   [#"(?i)^k\.?h" {:journalNo "57" :journalEntry "502", :journalYear "1934"}]
   [#"(?i)^k\.?k\.?s"
    {:journalNo "83" :journalEntry "930", :journalYear "1999"}]
   [#"(?i)^k\.?k\.?w"
    {:journalNo "90" :journalEntry "557", :journalYear "1997"}]
   [#"(?i)^k\.?k" {:journalNo "88" :journalEntry "553", :journalYear "1997"}]
   [#"(?i)^k\.?m" {:journalNo "138" :journalEntry "1545", :journalYear "2001"}]
   [#"(?i)^k\.?p\.?a"
    {:journalNo "30" :journalEntry "168", :journalYear "1960"}]
   [#"(?i)^k\.?p\.?c"
    {:journalNo "43" :journalEntry "296", :journalYear "1964"}]
   [#"(?i)^k\.?p\.?k"
    {:journalNo "89" :journalEntry "555", :journalYear "1997"}]
   [#"(?i)^k\.?p\.?w"
    {:journalNo "106" :journalEntry "1148", :journalYear ""}]
   [#"(?i)^k\.?p" {:journalNo "24" :journalEntry "141", :journalYear "1974"}]
   [#"(?i)^k\.?r\.?o" {:journalNo "9" :journalEntry "59", :journalYear "2001"}]
   [#"(?i)^k\.?s\.?h"
    {:journalNo "94" :journalEntry "1037", :journalYear "2000"}]
   [#"(?i)^k\.?w" {:journalNo "12" :journalEntry "114", :journalYear "1971"}]
   [#"(?i)^k\.?z" {:journalNo "82" :journalEntry "598", :journalYear "1933"}]
   [#"(?i)^u\.?s\.?p"
    {:journalNo "98" :journalEntry "1070", :journalYear "2001"}]
   [#"(?i)^ustawy o TK"
    {:journalNo "102" :journalEntry "643", :journalYear "1997"}]
   [#"(?i)^ustawy o Trybunale Konstytucyjnym"
    {:journalNo "102" :journalEntry "643", :journalYear "1997"}]
   [#"(?i)^ustawy o komornikach"
    {:journalNo "133" :journalEntry "882", :journalYear "1997"}]
   [#"(?i)^ustawy o ochronie konkurencji"
    {:journalNo "50" :journalEntry "331", :journalYear "2007"}]])

(def dictionary-for-acts-greedy
  (concat dictionary-for-acts-strict
  [[#"(?i)^pzp" {:journalNo "19" :journalEntry "177", :journalYear "2004"}]
   [#"(?i)^ustawy pzp"
    {:journalNo "19" :journalEntry "177", :journalYear "2004"}]
   [#"(?i)^ustawy prawo zamówień publicznych"
    {:journalNo "19" :journalEntry "177", :journalYear "2004"}]
   [#"(?i)^Prawo zamówień publicznych"
    {:journalNo "19" :journalEntry "177", :journalYear "2004"}]
   [#"(?i)^prawa zamówień publicznych"
    {:journalNo "19" :journalEntry "177", :journalYear "2004"}]
   [#"(?i)^prawa o adwokat"
    {:journalNo "16" :journalEntry "124", :journalYear "1982"}]]))

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

(defn extract-dictionary-case [tokens dictionary]
  (let [
        txt (tokens-to-string tokens)
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
      tokens
      dictionary-record)))

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

(defn extract-act-coords-greedy [tokens dictionary]
  (cond
   (some #{"Dz.U"} tokens)
   (extract-act-coords-journal-with-dot tokens)
   (some #{"Dz"} tokens)
   (extract-year-journal-nmb-and-entry tokens)
   :else
   (extract-dictionary-case tokens dictionary)))

(defn extract-act-coords-strict [tokens dictionary]
  (let [
        extracted-by-dictionary
          (extract-dictionary-case tokens dictionary-for-acts-strict)
        ]
    (if (map? extracted-by-dictionary)
      extracted-by-dictionary
      (cond
       (some #{"Dz.U"} tokens)
       (extract-act-coords-journal-with-dot tokens)
       (some #{"Dz"} tokens)
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

(defn load-dictionary [path]
  (let [
        txt (slurp path)
        lines (str/split txt (re-pattern system-newline))
        trimmed-lines
          (map
           #(subs % 1 (dec (count %)))
           lines)
        pattern
          (re-pattern
           (str "\"" csv-delimiter "\""))
        records
          (map
           #(str/split % pattern)
           trimmed-lines)
        dictionary
          (map
           #(vector
             (re-pattern
              (replace-several (str "(?i)" (nth % 0))
                               #"\(" "\\("
                               #"\)" "\\)"))
             {:journalYear (nth % 1)
              :journalNo (nth % 2)
              :journalEntry (nth % 3)})
           records)
        ]
    dictionary))

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

(defn extract-law-links [s dictionary extract-act-coords-fn]
  (let [
        preprocessed (preprocess s)
        merged-dictionary (concat dictionary dictionary-for-acts-greedy)
        txt (replace-several preprocessed
                             #"art\." " art. "
                             #"ust\." " ust. "
                             #"§" " § "
                             #"pkt" " pkt "
                             #"zd\." " zd. "
                             #"poz\." " poz. "
;;                              #"Dz\.U\." "Dz. U."
                             )
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
        act-coords
          (handle-w-zwiazku-z
           (map #(extract-act-coords-fn % merged-dictionary)
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

;;         _ (print-art-act-texts
;;            tokens correct-art-coords-ranges inter-coords-ranges)
        ]
    (zipmap
     [:extracted-links :orphaned-links]
     [ (into [] extracted-links-cleansed)
       (into []
             (mapcat get-data-for-orphaned-link orphaned-links))])))

(defn extract-law-links-greedy [s dictionary]
  (extract-law-links s dictionary extract-act-coords-greedy))

(defn extract-law-links-strict [s dictionary]
  (extract-law-links s dictionary extract-act-coords-strict))
