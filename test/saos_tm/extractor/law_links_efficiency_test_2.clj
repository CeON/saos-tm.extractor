(ns saos-tm.extractor.law-links-efficiency-test-2
  (:require [clojure.test :refer :all]
            [saos-tm.extractor.common-test :as common-test]))

(deftest law-links-efficiency-test-2
  (common-test/law-links-efficiency-test
   "law-links-answers/" "txt2" "law2"
   common-test/get-benchmark-records
   common-test/law-links-extract-all-dictionaries
   common-test/links-preprocess
   0.859 0.803
   0.949 0.574
   0.856 0.521
   common-test/get-csv-for-extracted-link
   common-test/log-results-with-case-nmbs))
