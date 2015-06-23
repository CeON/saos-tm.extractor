(ns saos-tm.extractor.rich-judgment-links-efficiency-test
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]
   [clojure-csv.core :as csv]
   [saos-tm.extractor.rich-judgment-links :as rich-judgment-links]
   [saos-tm.extractor.judgment-links :as judgment-links]
   [saos-tm.extractor.common :as common]
   [saos-tm.extractor.common-test :as common-test]))

(defn ^:private get-benchmark-rich-judgment-links [ext-files]
  (let [
        rich-judgments-links      (map csv/parse-csv ext-files)
        rich-judgments-links-sets (map set rich-judgments-links)
        ]
    rich-judgments-links-sets))

(defn ^:private conv-coll-to-csv-line [coll not-used]
  (apply str "\""
         (str/join (str "\"" common/csv-delimiter "\"") coll)
         "\"" common/system-newline))

(defn ^:private rich-judgment-links-extract [txt-files]
  (map
    #(set (map vals (rich-judgment-links/extract-ref-judgments %)))
    txt-files))

(defn ^:private extract-own-signature-line [s]
  (first
   (re-find
    (re-pattern
     (str "(?i)(sygn\\.|sygnatur)[^" common/system-newline "]*"))
    s)))

(defn ^:private remove-signature-headlines
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

(defn ^:private remove-own-signature [s]
  (let [
        case-nmbs (judgment-links/extract-all-signatures s)
        regex
          (re-pattern
           (str/join "|"
                     (map common/conv-str-to-regex case-nmbs)))
        matches-with-starts-ends
          (rich-judgment-links/sort-regexes
           (common/get-regex-matches-with-starts-ends-maps
            regex s)
           :start)
        first-case-nmb (:regex (first matches-with-starts-ends))
        first-case-nmb-regex
          (re-pattern
           (common/replace-several first-case-nmb
                                   #"\s" "."
                                   #"/" "."))
        ]
    (str/replace s first-case-nmb-regex " ")))

(defn ^:private rich-links-preprocess [coll]
  (let [
        without-own-signatures (map remove-own-signature coll)
        without-page-nmbs
          (map common-test/remove-page-nmbs without-own-signatures)
        ]
  without-page-nmbs))

(deftest rich-judgment-links-efficiency-test
  (common-test/judgment-links-efficiency-test
   "rich-judgment-links-answers/" "txt1" "rich-jdg"
   get-benchmark-rich-judgment-links
   rich-judgment-links-extract rich-links-preprocess
   0.929 0.931
   conv-coll-to-csv-line common-test/log-results-without-signatures))
