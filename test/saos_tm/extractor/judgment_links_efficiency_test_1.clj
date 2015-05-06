(ns saos-tm.extractor.judgment-links-efficiency-test-1
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.common :refer :all]
   [saos-tm.extractor.common-test :refer :all]))

(deftest judgment-links-efficiency-test-1
  (links-efficiency-test
   "txt1" "jdg1" get-benchmark-signatures judgment-links-extract
   links-preprocess
   0.991 0.993
   signature-to-csv log-results-without-signatures))
