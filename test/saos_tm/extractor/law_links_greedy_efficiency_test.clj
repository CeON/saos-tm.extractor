(ns saos-tm.extractor.law-links-greedy-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test
  (law-links-efficiency-test
   "law" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.723 0.87
   0.957 0.823
   0.732 0.65
   get-csv-for-extracted-link log-results-with-signatures))
