(ns saos-tm.extractor.law-links-greedy-efficiency-test-2
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test-2
  (law-links-efficiency-test
   "txt2" "law2" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.836 0.766
   0.951 0.565
   0.85 0.508
   get-csv-for-extracted-link log-results-with-signatures))
