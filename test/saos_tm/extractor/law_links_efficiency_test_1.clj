(ns saos-tm.extractor.law-links-efficiency-test-1
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :as common-test]))

(deftest law-links-efficiency-test-1
  (common-test/law-links-efficiency-test
   "law-links-answers/" "txt1" "law1"
   common-test/get-benchmark-records
   common-test/law-links-extract-all-dictionaries
   common-test/links-preprocess
   0.936 0.834
   0.971 0.809
   0.949 0.794
   common-test/get-csv-for-extracted-link
   common-test/log-results-with-case-nmbs))
