(ns saos-tm.extractor.law-links-greedy-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test
  (law-links-efficiency-test
   "law" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.814 0.838
   0.956 0.772
   0.87 0.722
   get-csv-for-extracted-link log-results-with-signatures))
