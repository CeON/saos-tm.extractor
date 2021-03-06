(ns saos-tm.extractor.judgment-links-efficiency-test-1
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.common-test :as common-test]))

(deftest judgment-links-efficiency-test-1
  (common-test/judgment-links-efficiency-test
   "judgment-links-answers/" "txt1" "jdg1"
   common-test/get-benchmark-case-nmbs common-test/judgment-links-extract
   common-test/links-preprocess
   0.991 0.993
   common-test/case-nmb-to-csv common-test/log-results-without-case-nmbs))
