(ns saos-tm.extractor.law-links-efficiency-test-1
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :as common-test]))

(deftest law-links-efficiency-test-1
  (common-test/law-links-efficiency-test
   "law-links-answers/" "txt1" "law1"
   common-test/get-benchmark-records common-test/law-links-extract-greedy
   common-test/links-preprocess
   0.927 0.83
   0.971 0.806
   0.947 0.791
   common-test/get-csv-for-extracted-link
   common-test/log-results-with-signatures))
