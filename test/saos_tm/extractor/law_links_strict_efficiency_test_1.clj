(ns saos-tm.extractor.law-links-strict-efficiency-test-1
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-strict-efficiency-test-1
  (law-links-efficiency-test
   "txt1" "law1" get-benchmark-records law-links-extract-strict
   links-preprocess
   0.844 0.834
   0.963 0.521
   0.910 0.494
   get-csv-for-extracted-link log-results-with-signatures))
