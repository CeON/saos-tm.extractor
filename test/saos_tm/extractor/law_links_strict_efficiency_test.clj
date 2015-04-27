(ns saos-tm.extractor.law-links-strict-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-strict-efficiency-test
  (law-links-efficiency-test
   "law" get-benchmark-records law-links-extract-strict
   links-preprocess
   0.844 0.838
   0.962 0.513
   0.901 0.483
   get-csv-for-extracted-link log-results-with-signatures))
