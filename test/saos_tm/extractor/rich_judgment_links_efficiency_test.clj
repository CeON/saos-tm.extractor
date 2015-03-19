(ns saos-tm.extractor.rich-judgment-links-efficiency-test
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.rich-judgment-links :refer :all]
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

(deftest judgment-links-efficiency-test
  (links-efficiency-test "rich-jdg" get-benchmark-rich-judgment-links
    rich-judgment-links-extract -0.01 -0.01 conv-coll-to-csv-line))
