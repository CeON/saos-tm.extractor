(ns saos-tm.extractor.law-links-strict-efficiency-test-2
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-strict-efficiency-test-2
  (law-links-efficiency-test
   "txt2" "law2" get-benchmark-records law-links-extract-strict
   links-preprocess
   0.828 0.859
   0.867 0.438
   0.755 0.391
   get-csv-for-extracted-link log-results-with-signatures))
