(ns saos-tm.extractor.rich-judgment-links
  (:require
   [ clojure.string :as str ])
  (:import java.io.File)
  (:gen-class))

(defn extract-ref-judgments [s]
  '#{{:judgmentDate nil :court nil :judgmentType nil :caseNumber nil}})
