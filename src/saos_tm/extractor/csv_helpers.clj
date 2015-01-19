(ns saos-tm.extractor.csv-helpers
  (:require
   [clojure.string :as str]
   [saos-tm.extractor.common :refer :all]
)
  (:gen-class))

(defn get-nth-args [csv-string]
  (let [
        lines (str/split-lines csv-string)
        nth-args
          (map
           #(first
             (str/split % (re-pattern csv-delimiter)))
           lines)
        without-quots
          (map
           #(apply str (drop 1 (drop-last 1 %)))
           nth-args)
        ]
    without-quots))
