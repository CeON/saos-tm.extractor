(ns saos-tm.extractor.law-links-greedy-efficiency-test
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [saos-tm.extractor.common :refer :all]))

(deftest law-links-greedy-efficiency-test
  (law-links-efficiency-test
   "law" get-benchmark-records law-links-extract-greedy
   links-preprocess
   0.893 0.846
   0.959 0.802
   0.925 0.779
   get-csv-for-extracted-link log-results-with-signatures))
