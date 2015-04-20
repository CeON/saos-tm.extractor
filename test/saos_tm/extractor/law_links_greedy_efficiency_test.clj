(ns saos-tm.extractor.law-links-greedy-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test
  (law-links-efficiency-test
   "law" get-benchmark-records law-links-extract-greedy
   identity
   0.705 0.874
   0.951 0.756
   0.647 0.535
   get-csv-for-extracted-link log-results-with-signatures))
