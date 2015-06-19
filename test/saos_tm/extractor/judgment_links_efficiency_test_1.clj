(ns saos-tm.extractor.judgment-links-efficiency-test-1
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.common-test :as common-test]))

(deftest judgment-links-efficiency-test-1
  (common-test/links-efficiency-test
   "txt1" "jdg1"
   common-test/get-benchmark-signatures common-test/judgment-links-extract
   common-test/links-preprocess
   0.991 0.993
   common-test/signature-to-csv common-test/log-results-without-signatures))
