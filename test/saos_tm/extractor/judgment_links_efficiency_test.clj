(ns saos-tm.extractor.judgment-links-efficiency-test
  (:require
   [ clojure.test :refer :all ]
   [ clojure.string :as str ]
   [ clojure.set :refer :all ]
   [ saos-tm.extractor.common :refer :all ]
   [ saos-tm.extractor.common-test :refer :all ]
   [ saos-tm.extractor.judgment-links :refer :all ]
   [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]))

(defn get-benchmark-signatures [ext-files]
  (let [
        jdg-signatures
          (map
           #(get-signature %)
           ext-files)
        benchmark-signatures
          (map
           #(set (remove empty? (map str/trim %)))
           jdg-signatures)
        ]
    benchmark-signatures))

(defn judgment-links-extract [txt-files]
  (map
    #(extract-all-signatures %)
    txt-files))

(defn signature-to-csv [signature not-used]
  (apply str "\"" signature "\"" system-newline))

(deftest judgment-links-efficiency-test
  (links-efficiency-test
   "jdg" get-benchmark-signatures judgment-links-extract
   links-preprocess
   0.991 0.993
   signature-to-csv log-results-without-signatures))
