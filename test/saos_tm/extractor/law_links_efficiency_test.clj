(ns saos-tm.extractor.law-links-efficiency-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str ]
            [clojure.java.io :as io]
            [clojure-csv.core :refer :all ]
            [saos-tm.extractor.common :refer :all]
            [saos-tm.extractor.law-links :refer :all]
            [saos-tm.extractor.common-test :refer :all ]
            [langlab.core.parsers :refer [ lg-split-tokens-bi ] ]))

(defn read-law-links-to-maps [file-data]
  (let [
          data (parse-csv file-data)
    ]
    (into #{} (map
      #(zipmap
        [:art :act]
        [(zipmap
          [:art :par :ust :pkt :zd :lit]
          (take 6 %))
         (zipmap
          [:year :nr :poz]
          (take-last 3 %))])
      data))))

(defn get-benchmark-records [files]
  (map
    read-law-links-to-maps
    files))

(defn law-links-extract [txt-files]
  (let [
        dictionary (load-dictionary (io/resource "act_dictionary.txt"))
        ]
    (map
      #(into #{} (:extracted-links (extract-law-links % dictionary)))
      txt-files)))

(deftest law-links-efficiency-test
  (links-efficiency-test ".law" #"\.law" get-benchmark-records
    law-links-extract 0.59155 0.5245 get-csv-for-extracted-link))
       