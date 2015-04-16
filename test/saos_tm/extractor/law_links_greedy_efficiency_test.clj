(ns saos-tm.extractor.law-links-greedy-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test
  (links-efficiency-test
   "law" get-benchmark-records law-links-extract-greedy
   identity 0.649 0.533
   get-csv-for-extracted-link log-results-with-signatures))
