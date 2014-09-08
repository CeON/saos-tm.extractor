(ns saos-tm.extractor.judgment-links
  (:require
    [ saos-tm.extractor.common :as common ]
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    )
  (:import java.io.File)
  (:gen-class))

(defn extract-signatures-osp [s]
  (map #(str/trim (str %))
    (map #(first %)
      (re-seq #"[XVI]*\s*[0-9]*\s*[A-Za-z]+\s+[0-9]+(/[0-9]+)+" s))))

(defn extract-all-signatures [s]
  (let [
          from-regex s
        ]
        from-regex))

(defn extract-signatures-from-file
  [input-file-path output-file-path]
  (let [
          input-txt (slurp input-file-path)
          signatures (extract-all-signatures input-txt)
        ]
  (spit output-file-path
    signatures)))
 