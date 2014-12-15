(ns saos-tm.extractor.common
  (:require
    [ clojure.string :as str ]
    [ clojure.xml :as xml ]
    [ clojure.zip :as zip ]
    [ clojure.set :refer :all ]
    [ langlab.core.parsers :refer :all ])
  (:import java.io.File)
  (:gen-class))

(def csv-delimiter ",")
(def ^String system-newline
  (System/getProperty "line.separator"))

(defn split-to-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

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

(defn get-measure [true-positives-count elements-count]
  (if
    (= elements-count 0)
    nil
    (float (/ true-positives-count elements-count))))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn zip-str [s]
  (zip/xml-zip
    (xml/parse
      (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn matches? [s re]
  (not-nil? (re-matches re s)))

(defn dexmlise [s]
  (when (not-nil? s)
    (str/replace s #"\<((?![\<\>])[\s\S])*\>" "")))

(defn find-first [f coll]
  (first (filter f coll)))

(defn filter-ending-with [ss s]
  (sort
    (filter
      #(.endsWith (str %) s)
      ss)))

(defn get-regex-match [regex following-text-regex s]
  (re-find
    (re-pattern
      (str regex following-text-regex))
    s))

(defn get-first-regex-match [coll following-text-regex s]
  (let [
          matches
            (map
              #(get-regex-match % following-text-regex s)
              coll)
          match
            (find-first
             #(not-nil? %)
             matches)
          match
            (if (string? match)
              match
              (first match))
        ]
    match))

(defn get-first-regex-match-case-ins [coll following-text-regex s]
  (get-first-regex-match
    (map #(str "(?i)" %) coll)
    following-text-regex
    s))

(defn str-to-regex [s]
  (re-pattern
   (replace-several s
                   #"\." (str "\\" "."))))
