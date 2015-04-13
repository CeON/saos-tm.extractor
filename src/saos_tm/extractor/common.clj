(ns saos-tm.extractor.common
  (:require
    [clojure.string :as str]
    [clojure.set :refer :all]
    [langlab.core.parsers :refer :all])
  (:import [java.io File]
           [org.apache.commons.io IOUtils]
           [org.apache.tika.parser Parser ParseContext]
           [org.apache.tika.parser.html HtmlParser]
           [org.apache.tika.language LanguageIdentifier]
           [org.apache.tika.metadata Metadata]
           [org.apache.tika Tika]
           [org.apache.tika.sax BodyContentHandler]))

(def csv-delimiter ",")

(def ^String system-newline
  (System/getProperty "line.separator"))

(defn get-art-coords-csv [art-coords]
  (let [
        art-nr (:art art-coords)
        par-nr (:par art-coords)
        ust-nr (:ust art-coords)
        pkt-nr (:pkt art-coords)
        zd-nr (:zd art-coords)
        lit-nr (:lit art-coords)
        ]
    (apply str
           "\"" art-nr "\"" csv-delimiter
           "\"" par-nr "\"" csv-delimiter
           "\"" ust-nr "\"" csv-delimiter
           "\"" pkt-nr "\"" csv-delimiter
           "\"" zd-nr "\"" csv-delimiter
           "\"" lit-nr "\"" csv-delimiter)))

(defn split-to-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

;; for debugging
(defn print-if-contains [s element]
  (if (substring? element s) (prn s)))
(defn print-that [s]
  (doall (prn) (prn)) (prn s) (doall (prn) (prn)))

(defn extract-nmbs-and-ranges [ s ]
  (map #(str/trim %)
    (str/split s #",| i |(oraz)")))

(defn get-coords-names [is-art is-par is-ust is-pkt is-zd is-lit]
  (filter #(not= "" %)
    [(if is-art "art" "")
     (if is-par "par" "")
     (if is-ust "ust" "")
     (if is-pkt "pkt" "")
     (if is-zd "zd." "")
     (if is-lit "lit." "")]))

(defn convert-ranges-to-single-records [record]
  [(flatten (record "art"))
  (flatten (record "par"))
  (flatten (record "ust"))
  (flatten (record "pkt"))
  (flatten (record "zd."))
  (flatten (record "lit."))])

(defn change-empty [value]
  (if (empty? value)
    ["0"]
    value))

(defn convert-empties [art-coords]
  (map #(change-empty %) art-coords))

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
      more (cartesian-product (rest colls))]
      (cons x more))))

(defn remove-trailing-conjunction [s]
  (let [
          tokens (split-to-tokens s)
          last-token (last tokens)
          ]
          (if (or (= last-token "i") (= last-token "z") (= last-token "oraz"))
            (str/trim
              (apply str
                (drop-last (count last-token) s)))
            (str/trim s))))

(defn extract-coords-for-single-art [s]
  (cartesian-product
    (convert-empties
      (convert-ranges-to-single-records
        (zipmap
          (get-coords-names
            (substring? "art." s)
            (substring? "§" s)
            (substring? "ust" s)
            (substring? "pkt" s)
            (substring? "zd" s)
            (substring? "lit" s))
          (map extract-nmbs-and-ranges
            (drop 1
              (str/split s #"art\.|§|ust\.|ust|pkt|zd\.|zd|lit\.|lit"))))))))

(defn replace-several [content & replacements]
  (let [replacement-list (partition 2 replacements)]
    (reduce #(apply str/replace %1 %2) content replacement-list)))

(defn extract-coords [s]
  (let [
          trimmed (str/trim s)
          separate-art-coords
            (if (substring? "art." trimmed)
              (map
                #(apply str "art." %)
                (drop 1 (str/split trimmed #"art\.")))
              [trimmed])
          separate-art-coords-trimmed
            (map
              #(str/trim %)
              separate-art-coords)
          without-conjunctions
            (map
              #(remove-trailing-conjunction %)
              separate-art-coords-trimmed)
          ]
          (mapcat extract-coords-for-single-art without-conjunctions)))

(defn get-year-from-act-name [s]
  (let [
        pattern (last (re-seq #"\d+\s+r" s))
        ]
        (when
          (not-empty pattern)
          (apply str
            (re-seq #"[\d]+" pattern)))))

(defn get-year-of-law-act [s]
  (let [
          pattern (re-find #"Dz\.\s+U\.\s+z?\s+\d+\s+r" s)
          year
            (if
              (not-empty pattern)
              (apply str
                (re-seq #"[\d]+" pattern))
              (get-year-from-act-name
                (first
                  (str/split s #"Dz\."))))
            ]
            year))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn seq-to-csv [coll]
  (let [
          with-delim-at-the-end
            (str/join ""
              (map
                #(apply str "\"" % "\"" csv-delimiter)
                coll))
          with-newline-at-the-end
            (str
              (apply str (drop-last with-delim-at-the-end))
              system-newline)
            ]
            with-newline-at-the-end))

(def not-nil? (complement nil?))

(def not-empty? (complement empty?))

(defn get-measure [true-positives-count elements-count]
  (if
    (= elements-count 0)
    nil
    (float (/ true-positives-count elements-count))))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn matches? [s re]
  (not-nil? (re-matches re s)))

(def not-matches? (complement matches?))

(defn dexmlise [s]
  (when (not-nil? s)
    (str/replace s #"\<((?![\<\>])[\s\S])*\>" "")))

(defn find-first [f coll]
  (first (filter f coll)))

(defn get-regex-matches [re s func]
  (loop [match (re-matcher re s)
         result {}]
    (if (.find match)
      (recur match
             (func match result))
      result)))

(defn start-func [match result]
  (assoc result (.start match) (.group match)))

(defn get-regex-matches-with-starts [re s]
  (get-regex-matches re s start-func))

(defn start-end-func [match result]
  (assoc result [(.start match) (.end match)] (.group match)))

(defn get-regex-matches-with-starts-ends [re s]
  (get-regex-matches re s start-end-func))

(defn start-end-map-func [match result]
  (concat
   result
   [(zipmap
     [:start :end :regex]
     [(.start match) (.end match) (.group match)])]))

(defn get-regex-matches-with-starts-ends-maps [re s]
  (get-regex-matches re s start-end-map-func))

(defn get-sorted [func re s]
  (let [
        regexes (func re s)
        sorted (sort regexes)
        ]
    sorted))

(defn get-regex-matches-with-starts-ends-sorted [re s]
  (get-sorted get-regex-matches-with-starts-ends re s))

(defn get-regex-match [func regex following-text-regex s]
  (get-sorted func
   (re-pattern
    (str regex following-text-regex))
   s))

(defn get-matches [func regexes following-text-regex s]
  (map
   #(get-regex-match func % following-text-regex s)
   regexes))

(defn find-first-not-empty [coll]
  (find-first #(not-empty? %) coll))

(defn get-first-if-groups [match]
  (if (string? match) match (first match)))

(defn get-closest-regex-match [regexes following-text-regex s]
  (let [
        matches-with-positions
          (get-matches
           get-regex-matches-with-starts-ends regexes following-text-regex s)
        matches-with-positions-sorted
          (sort #(compare (first %1) (first %2)) matches-with-positions)
        first-match (find-first-not-empty matches-with-positions-sorted)
        first-match-str (get-first-if-groups first-match)
        ]
    first-match-str))

(defn get-regex-match-case-ins [func regexes following-text-regex s]
  (func
    (map #(str "(?i)" %) regexes) following-text-regex s))

(defn get-regex-match-case-sen [func regexes following-text-regex s]
  (func regexes following-text-regex s))

(defn get-closest-regex-match-case-ins [regexes following-text-regex s]
  (get-regex-match-case-ins
   get-closest-regex-match regexes following-text-regex s))

(defn get-closest-regex-match-case-sen [regexes following-text-regex s]
  (get-regex-match-case-sen
   get-closest-regex-match regexes following-text-regex s))

(defn remove-all-html-tags [s]
  (str/replace s
               (re-pattern (str "<[^>]*>")) " "))

(defn remove-html-tags-other-than [tag-name s]
  (str/replace s
               (re-pattern (str "<(?!/?" tag-name ")((?!>)[\\s\\S])*>")) " "))

(defn remove-html-tags-other-than-span [s]
  (remove-html-tags-other-than "span" s))

(defn conv-html-to-text [ ^String s]
  (let [
          istream (IOUtils/toInputStream s "UTF-8");
          parser (HtmlParser.)
          context (ParseContext.)
          metadata (Metadata.)
          handler (BodyContentHandler. (.length s))
          ]
      (.set context Parser parser)
      (.parse parser istream handler metadata context)
      (.toString  handler)))

(defn get-csv-for-extracted-link [link signature]
  (let [
        art (:art link)
        act (:act link)
        ]
    (apply str (get-art-coords-csv art)
           "\"" signature "\"" csv-delimiter
           "\"" (:year act) "\"" csv-delimiter
           "\"" (:journalNo act) "\"" csv-delimiter
           "\"" (:entry act) "\"" system-newline)))

(defn remove-hard-spaces [s]
  (str/replace s #"\u00A0" " "))

(defn remove-double-spaces [s]
  (str/replace s #"\s+" " "))

(defn remove-newlines [s]
  (let [
        without-double-slash-newlines (str/replace s #"\\n" " ")
        without-newlines
          (str/replace without-double-slash-newlines
                       (re-pattern system-newline) " ")
        ]
    without-newlines))

(defn unsplit-words-across-lines [s]
  (let [
        without-split-numbers
          (str/replace s
                       (re-pattern (str "(?<=\\d-)" system-newline "(?=\\d)"))
                       "")
        ]
  (str/replace without-split-numbers (str "-" system-newline) "")))

(defn preprocess [s]
  (let [
        without-split-words (unsplit-words-across-lines s)
        without-tags (remove-all-html-tags without-split-words)
        without-hard-spaces (remove-hard-spaces without-tags)
        without-newlines (remove-newlines without-hard-spaces)
        without-double-spaces (remove-double-spaces without-newlines)
        ]
    without-double-spaces))
