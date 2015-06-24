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
    #(set (map vals (rich-judgment-links/extract-rich-judgment-links %)))
    txt-files))

(defn ^:private extract-own-case-nmb-line [s]
  (first
   (re-find
    (re-pattern
     (str "(?i)(sygn\\.|sygnatur)[^" common/system-newline "]*"))
    s)))

(defn ^:private remove-case-nmb-headlines
  "Some law acts have a case number in headline of every page.
  This makes it difficult to extract other case numbers
  when they appear on two pages."
  [s]
  (let [
        own-case-nmb-line (extract-own-case-nmb-line s)
        without-own-case-nmb-lines
          (if (nil? own-case-nmb-line)
            s
            (str/replace s (str/trim own-case-nmb-line) " "))
        ]
    without-own-case-nmb-lines))

(defn ^:private remove-own-case-nmb [s]
  (let [
        case-nmbs (judgment-links/extract-judgment-links s)
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
        without-own-case-nmbs (map remove-own-case-nmb coll)
        without-page-nmbs
          (map common-test/remove-page-nmbs without-own-case-nmbs)
        ]
  without-page-nmbs))

(deftest rich-judgment-links-efficiency-test
  (common-test/judgment-links-efficiency-test
   "rich-judgment-links-answers/" "txt1" "rich-jdg"
   get-benchmark-rich-judgment-links
   rich-judgment-links-extract rich-links-preprocess
   0.929 0.931
   conv-coll-to-csv-line common-test/log-results-without-case-nmbs))
