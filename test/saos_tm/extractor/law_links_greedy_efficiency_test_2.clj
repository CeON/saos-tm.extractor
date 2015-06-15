(ns saos-tm.extractor.law-links-greedy-efficiency-test-2
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test-2
  (law-links-efficiency-test
   "txt2" "law2" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.847 0.831
   0.944 0.586
   0.83 0.53
   get-csv-for-extracted-link log-results-with-signatures))
