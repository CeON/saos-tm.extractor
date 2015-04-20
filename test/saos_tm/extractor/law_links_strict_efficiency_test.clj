(ns saos-tm.extractor.law-links-strict-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-strict-efficiency-test
  (law-links-efficiency-test
   "law" get-benchmark-records law-links-extract-strict
   identity
   0.84 0.834
   0.959 0.515
   0.898 0.485
   get-csv-for-extracted-link log-results-with-signatures))
