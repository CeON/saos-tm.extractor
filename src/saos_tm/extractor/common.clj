(ns saos-tm.extractor.common
  (:require
    [ clojure.string :as str ]
    [ clojure.set :refer :all ]
    [ langlab.core.parsers :refer :all ])
  (:import java.io.File)
  (:gen-class))

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

(defn get-csv-for-extracted-link [link signature]
  (let [
        art (:art link)
        act (:act link)
        ]
    (apply str (get-art-coords-csv art)
           "\"" signature "\"" csv-delimiter
           "\"" (:year act) "\"" csv-delimiter
           "\"" (:nr act) "\"" csv-delimiter
           "\"" (:poz act) "\"" system-newline)))


(defn split-to-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

;; for debugging
(defn print-if-contains [s element]
  (if (substring? element s) (prn s)))

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

(defn filter-ending-with [ss s]
  (sort
    (filter
      #(.endsWith (str %) s)
      ss)))

(defn re-pos [re s]
  (loop [m (re-matcher re s) res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn get-regex-match [func regex following-text-regex s]
  (func
   (re-pattern
    (str regex following-text-regex))
   s))

(defn get-matches [func regexes following-text-regex s]
  (map
   #(get-regex-match func % following-text-regex s)
   regexes))

(defn find-first-not-nil [coll]
  (find-first #(not-nil? %) coll))

(defn find-first-not-empty [coll]
  (find-first #(not-empty? %) coll))

(defn get-first-if-groups [match]
  (if (string? match) match (first match)))

(defn get-first-regex-match [regexes following-text-regex s]
  (let [
        matches (get-matches re-find regexes following-text-regex s)
        match (find-first-not-nil matches)
        match (get-first-if-groups match)
        ]
    match))

(defn get-closest-regex-match [regexes following-text-regex s]
  (let [
        matches-positions (get-matches re-pos regexes following-text-regex s)
        matches-positions
          (sort #(compare (first %1) (first %2)) matches-positions)
        match-pos (find-first-not-empty matches-positions)
        match-pos (get-first-if-groups match-pos)
        ]
    match-pos))

(defn get-regex-match-case-ins [func regexes following-text-regex s]
  (func
    (map #(str "(?i)" %) regexes) following-text-regex s))

(defn get-first-regex-match-case-ins [regexes following-text-regex s]
  (get-regex-match-case-ins
   get-first-regex-match regexes following-text-regex s))

(defn get-closest-regex-match-case-ins [regexes following-text-regex s]
  (get-regex-match-case-ins
   get-closest-regex-match regexes following-text-regex s))

(defn str-to-regex [s]
  (re-pattern
   (replace-several s
                    #"\." (str "\\")
                    #"\(" (str "\\" "(")
                    #"\)" (str "\\" ")")
                    )))

(defn remove-all-html-tags [s]
  (str/replace s
               (re-pattern (str "<[^>]*>")) " "))

(defn remove-html-tags-other-than [tag-name s]
  (str/replace s
               (re-pattern (str "<(?!/?" tag-name ")((?!>)[\\s\\S])*>")) " "))

(defn remove-html-tags-other-than-span [s]
  (remove-html-tags-other-than "span" s))
