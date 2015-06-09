(ns saos-tm.extractor.law-links-greedy-efficiency-test-1
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test-1
  (law-links-efficiency-test
   "txt1" "law1" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.894 0.85
   0.959 0.802
   0.925 0.779
   get-csv-for-extracted-link log-results-with-signatures))
