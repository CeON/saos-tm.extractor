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
    #"[IVXLCDM]+[\s\.]+[0-9]*[\s\.]*[a-zA-Z]*-?[a-zA-Z]*[\s\.]+\d+/\d+"
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
    (common/substring? "Nr" s)
    (common/substring? "OSNC" s)
    (= (last s) "/")))

(defn extract-signatures-tk [s]
  (map #(subs % 1 (count %))
    (remove #(not-tk? %)
      (re-seq #"[^IVXLCDM]\s[a-zA-Z]+\s+\d+/\d+" s))))

(defn extract-signatures-sn [s]
  (extract-and-trim #"[a-zA-Z]+\s+[IVXLCDM]+-\d+-\d+/\d+" s))

(defn extract-signatures-nsa [s]
  (extract-and-trim #"[IVXLCDM]+\s+[a-zA-Z]+/[a-zA-Z]+\s+\d+/\d+" s))

(defn regex-positions [re s]
  (loop [m (re-matcher re s)
    res {}]
    (if (.find m)
      (recur m (concat res [(.end m)]))
      res)))

(defn cleanse-signature [s]
  (let [
          last-char (last s)
          ]
          (if
            (some #{last-char} '(\, \; \. \)))
              (apply str (cleanse-signature (drop-last s)))
              s)))

(defn extract-signature-from-position [s start end]
  (let [
          tokens (str/split (subs s start end) #"\s")
          
          first-token-with-slash-index
          (first
            (common/indices
              #(common/substring? "/" %)
              tokens))
          last-token-with-slash-index
          (if (< first-token-with-slash-index 5)
            (if
              (common/substring? "/"
                (nth tokens (inc first-token-with-slash-index)))
              (inc first-token-with-slash-index)
              first-token-with-slash-index)
            first-token-with-slash-index)
          token (nth tokens last-token-with-slash-index)
          signature-end-position
          (+
            (str/.indexOf
              (subs s start end)
              token)
            (count token))
          signature
          (if (< first-token-with-slash-index 5)
            (subs s start (+ signature-end-position start))
            "")
          ]
          signature))

(defn extract-signatures-universal [s]
  (let [
          pos
          (distinct
            (concat
              (regex-positions #"sygn\.\s*(akt)?:?" s)
              (regex-positions #"sygnaturą\s*(akt)?:?" s)
              (regex-positions #"sygnaturze\s*(akt)?:?" s)
              (regex-positions #"sygnaturach\s*(akt)?:?" s)))
          pos
          (sort
            (concat pos [(count s)]))
        ]
        (for [i (range (dec (count pos)))]
          (extract-signature-from-position
            s (nth pos i) (nth pos (inc i))))))

(defn extract-all-signatures [s]
  (let [
          all
          (concat
            (extract-signatures-universal s)
            (extract-signatures-nsa s)
            (extract-signatures-sn s)
            (extract-signatures-tk s)
            (extract-signatures-osp s)
            (extract-signatures-kio-uzp s)
            (extract-signatures-kio-space s)
            (extract-signatures-kio-no-space s))
          clean (map #(cleanse-signature (str/trim %)) all)
          ]
          (distinct (remove #(= "" %) clean))))

(defn extract-signatures-from-file
  [input-file-path output-file-path]
  (let [
          input-txt (slurp input-file-path)
          signatures (extract-all-signatures input-txt)
          file-name
          (last
            (str/split
              input-file-path
              (re-pattern (str File/separatorChar))))
          csv 
          (apply str
            (common/seq-to-csv [file-name])
            (common/seq-to-csv [(count signatures)])
            (common/seq-to-csv signatures))
        ]
        (spit output-file-path csv)))
 