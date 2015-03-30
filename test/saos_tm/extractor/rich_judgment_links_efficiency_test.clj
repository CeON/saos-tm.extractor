(ns saos-tm.extractor.rich-judgment-links-efficiency-test
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.rich-judgment-links :refer :all]
   [saos-tm.extractor.judgment-links :refer :all]
   [saos-tm.extractor.common :refer :all]
   [saos-tm.extractor.common-test :refer :all]
   [clojure.string :as str]
   [clojure-csv.core :refer :all]))

(defn get-benchmark-rich-judgment-links [ext-files]
  (let [
        rich-judgments-links
          (map parse-csv ext-files)
        rich-judgments-links-sets
          (map set rich-judgments-links)
        ]
    rich-judgments-links-sets))

(defn conv-coll-to-csv-line [coll not-used]
  (apply str "\""
         (str/join (str "\"" csv-delimiter "\"") coll)
         "\"" system-newline))

(defn rich-judgment-links-extract [txt-files]
  (map
    #(set (map vals (extract-ref-judgments %)))
    txt-files))

(defn extract-own-signature-line [s]
  (first
   (re-find
    (re-pattern
     (str "(?i)(sygn\\.|sygnatur)[^" system-newline "]*"))
    s)))

(defn extract-own-signature [s]
  (let [
        own-signature-line (extract-own-signature-line s)
        own-signature
          (when (not-nil? own-signature-line)
            (cleanse-signature
             (second
              (str/split own-signature-line #"(?i)sygn[^\s]*\s*(akt:?)?"))))
        ]
    own-signature))

(defn remove-signature-headlines
  "Some law acts have a signature in headline of every page. This makes it
  difficult to extract some signatures when they appear on two pages."
  [s]
  (let [
        own-signature-line (extract-own-signature-line s)
        without-own-signature-lines
          (if (nil? own-signature-line)
            s
            (str/replace s (str/trim own-signature-line) " "))
        ]
    without-own-signature-lines))

(defn remove-own-signature [s]
  (let [
        case-nmbs (extract-all-signatures s)
        regex
          (re-pattern
           (str/join "|"
                     (map #(conv-str-to-regex %) case-nmbs)))
        matches-with-starts-ends
          (sort-regexes
           (get-regex-matches-with-starts-ends-maps regex s)
           :start)
        first-case-nmb (:regex (first matches-with-starts-ends))
        first-case-nmb-regex
          (re-pattern (str/replace first-case-nmb #"\s" "."))
        ]
    (str/replace s first-case-nmb-regex " ")))

(defn map-remove-own-signatures [coll]
  (map remove-own-signature coll))

(deftest rich-judgment-links-efficiency-test
  (links-efficiency-test
   "rich-jdg" get-benchmark-rich-judgment-links
   rich-judgment-links-extract map-remove-own-signatures
   0.58 0.61 conv-coll-to-csv-line))
