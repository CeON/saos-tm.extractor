(ns saos-tm.extractor.rich-judgment-links-efficiency-test
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.rich-judgment-links :refer :all]
   [saos-tm.extractor.judgment-links :refer :all]
   [saos-tm.extractor.common :refer :all]
   [saos-tm.extractor.common-test :refer :all]
   [clojure.string :as str]
   [clojure-csv.core :refer :all]
   ))

(defn get-rich-judgment-links-data [file-data]
  (parse-csv file-data))

(defn get-benchmark-rich-judgment-links [ext-files]
  (let [
        rich-judgments-links
          (map
           #(get-rich-judgment-links-data %)
           ext-files)
        rich-judgments-links-sets
          (map #(set %) rich-judgments-links)
        ]
    rich-judgments-links-sets))

(defn conv-coll-to-csv-line [coll not-used]
  (apply str "\""
         (str/join (str "\"" csv-delimiter "\"") coll)
         "\"" system-newline))

(defn get-vals [coll]
  (map #(vals %) coll))

(defn rich-judgment-links-extract [txt-files]
  (map
    #(set (get-vals (extract-ref-judgments %)))
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

(defn get-rich-judgment-links-efficiency
  [ext benchmark-records-fn extracted-records-fn
   precision-threshold recall-threshold result-to-csv-fn]
  (time
   (let [
         ext-dir (str links-test-data-path ext)
         ext-files (get-files-from-dir ext-dir)
         txt-files (get-files-from-dir (str links-test-data-path "txt/"))
         txt-files-without-own-signatures
           (map #(remove-own-signature %) txt-files)
         ext-files-names (list-file-names ext-dir)
         log-files-paths
           (map
            #(str log-data-path ext "/" %)
            ext-files-names)
         _ (.mkdir (java.io.File. (str log-data-path ext)))
         benchmark-items (benchmark-records-fn ext-files)
         extracted-items
           (extracted-records-fn txt-files-without-own-signatures)
         precisions-recalls
           (map get-precision-recall extracted-items benchmark-items)
         precisions
           (nils-to-zeros (get-elements :precision precisions-recalls))
         recalls
           (nils-to-zeros (get-elements :recall precisions-recalls))
         average-precision (get-average precisions)
         average-recall (get-average recalls)
         min-precision (apply min precisions)
         min-recall (apply min recalls)

         names-precs-recalls
           (sort
            #(compare (second %1) (second %2))
                 (map
                  vector
                  ext-files-names precisions recalls))
         _ (doseq [i names-precs-recalls] (println i))
         _ (println (str \newline "av. precision: " average-precision
                         " av. recall: " average-recall))
         _ (println (str "min precision: " min-precision
                         " min recall: " min-recall \newline))
         _
           (doall
            (map
             #(spit-all-csv result-to-csv-fn %1 %2)
             log-files-paths
             extracted-items))
         ]
     (is (> average-precision precision-threshold))
     (is (> average-recall recall-threshold)))))

(defn map-remove-own-signatures [coll]
  (map remove-own-signature coll))

(deftest rich-judgment-links-efficiency-test
  (links-efficiency-test
   "rich-jdg" get-benchmark-rich-judgment-links
   rich-judgment-links-extract map-remove-own-signatures
   0.58 0.61 conv-coll-to-csv-line))
