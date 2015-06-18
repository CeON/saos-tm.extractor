(ns saos-tm.extractor.law-links-greedy-efficiency-test-1
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test-1
  (law-links-efficiency-test
   "txt1" "law1" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.927 0.83
   0.971 0.806
   0.947 0.791
   get-csv-for-extracted-link log-results-with-signatures))
