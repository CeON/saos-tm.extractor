(ns saos-tm.extractor.judgment-links
  (:require
    [ saos-tm.extractor.common :as common ]
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    )
  (:import java.io.File)
  (:gen-class))

(defn extract-and-trim [reg s]
  (map #(str/trim (str/replace % "\n" " "))
    (re-seq reg s)))

(defn extract-signatures-osp [s]
  (extract-and-trim
    #"[IVXLCDM]+[\s\.]+[0-9]*[\s\.]*[a-zA-Z]*[\s\.]+\d+/\d+"
    s))

(defn extract-signatures-kio-no-space [s]
  (extract-and-trim #"KIO/UZP/\d+/\d+" s))

(defn extract-signatures-kio-space [s]
  (extract-and-trim #"KIO/UZP\s+\d+/\d+" s))

(defn extract-signatures-kio-uzp [s]
  (extract-and-trim #"UZP/ZO/\d+-\d+/\d+" s))

(defn not-tk? [s]
  (or
    (common/substring? "nr" s)
    (common/substring? "OSNC" s)
    (= (last s) "/")))

(defn extract-signatures-tk [s]
  (map #(subs % 1 (count %))
    (remove #(not-tk? %)
      (re-seq #"\s[a-zA-Z]+\s+\d+/\d+" s))))

(defn extract-signatures-sn [s]
  (extract-and-trim #"[a-zA-Z]+\s+[IVXLCDM]+-\d+-\d+/\d+" s))

(defn extract-signatures-nsa [s]
  (extract-and-trim #"[IVXLCDM]+\s+[a-zA-Z]+/[a-zA-Z]+\s+\d+/\d+" s))

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
 