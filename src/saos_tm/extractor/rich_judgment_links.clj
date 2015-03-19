(ns saos-tm.extractor.rich-judgment-links
  (:require
   [ clojure.string :as str ]
   [ saos-tm.extractor.judgment-links :refer :all ])
  (:import java.io.File)
  (:gen-class))

(defn extract-other-data [case-nmb s]
  (let [
        parts (str/split s (re-pattern case-nmb))
;;         _ (prn parts)
        ]
    ))

(defn extract-ref-judgments [s]
  (let [
        case-nmbs (extract-all-signatures s)
        other-judgment-links-data (extract-other-data (first case-nmbs) s)
        other-judgment-links-data (map #(extract-other-data % s) case-nmbs)
;;         _ (prn case-nmbs)
        ]
  #{{:judgmentDate nil :court nil :judgmentType nil :caseNumber nil}}))
