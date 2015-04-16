(ns saos-tm.extractor.law-links-strict-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-strict-efficiency-test
  (links-efficiency-test
   "law" get-benchmark-records law-links-extract-strict
   identity 0.891 0.483
   get-csv-for-extracted-link log-results-with-signatures))
