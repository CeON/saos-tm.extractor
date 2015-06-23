(ns saos-tm.extractor.judgment-links-efficiency-test-2
  (:require
   [clojure.test :refer :all]
   [saos-tm.extractor.common-test :as common-test]))

(deftest judgment-links-efficiency-test-2
  (common-test/judgment-links-efficiency-test
   "judgment-links-answers/" "txt2" "jdg2"
   common-test/get-benchmark-case-nmbs common-test/judgment-links-extract
   common-test/links-preprocess
   0.99 0.945
   common-test/case-nmb-to-csv common-test/log-results-without-case-nmbs))
