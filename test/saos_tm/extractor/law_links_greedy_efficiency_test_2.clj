(ns saos-tm.extractor.law-links-greedy-efficiency-test-2
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :as common-test]))

(deftest law-links-greedy-efficiency-test-2
  (common-test/law-links-efficiency-test
   "txt2" "law2"
   common-test/get-benchmark-records common-test/law-links-extract-greedy
   common-test/links-preprocess
   0.829 0.775
   0.949 0.574
   0.847 0.515
   common-test/get-csv-for-extracted-link
   common-test/log-results-with-signatures))
