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
           #(set (remove empty? (split-coll %)))
           jdg-signatures)
        ]
    benchmark-signatures))

(defn judgment-links-extract [txt-files]
  (map
    #(extract-all-signatures %)
    txt-files))

(defn signature-to-csv [signature signature-string]
  (apply str "\"" signature "\"""\n"))

(deftest judgment-links-efficiency-test
  (links-efficiency-test ".jdg" #"\.jdg" get-benchmark-signatures
    judgment-links-extract 0.942 0.983 signature-to-csv))
