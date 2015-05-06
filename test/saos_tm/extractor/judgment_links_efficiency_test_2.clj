(ns saos-tm.extractor.judgment-links-efficiency-test-2
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.common :refer :all]
   [saos-tm.extractor.common-test :refer :all]))

(deftest judgment-links-efficiency-test-2
  (links-efficiency-test
   "txt2" "jdg2" get-benchmark-signatures judgment-links-extract
   links-preprocess
   0.99 0.927
   signature-to-csv log-results-without-signatures))
