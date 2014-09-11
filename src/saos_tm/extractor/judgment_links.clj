(ns saos-tm.extractor.judgment-links
  (:require
    [ saos-tm.extractor.common :as common ]
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    [ clojure.set :refer :all]
    )
  (:import java.io.File)
  (:gen-class))

(defn extract [reg s]
  (set
    (map
      #(str/replace % "\n" " ")
      (re-seq reg s))))

(defn extract-signatures-osp [s]
  (extract
    #"[IVXLCDM]+[\s\.]+[0-9]*[\s\.]*[a-zA-Z]*-?[a-zA-Z]*[\s\.]+\d+/\d+"
    s))

(defn extract-signatures-kio-no-space [s]
  (extract #"KIO/UZP/\d+/\d+" s))

(defn extract-signatures-kio-space [s]
  (extract #"KIO/UZP\s+\d+/\d+" s))

(defn extract-signatures-kio-uzp [s]
  (extract #"UZP/ZO/\d+-\d+/\d+" s))

(defn not-tk? [s]
  (or
    (common/substring? "nr" s)
    (common/substring? "Nr" s)
    (common/substring? "OSNC" s)
    (= (last s) "/")))

(defn extract-signatures-tk [s]
  (set
    (map #(subs % 1 (count %))
      (remove #(not-tk? %)
        (extract #"[^IVXLCDM]\s[a-zA-Z]+\s+\d+/\d+" s)))))

(defn extract-signatures-sn [s]
  (extract #"[a-zA-Z]+\s+[IVXLCDM]+-\d+-\d+/\d+" s))

(defn extract-signatures-nsa [s]
  (extract #"[IVXLCDM]+\s+[a-zA-Z]+/[a-zA-Z]+\s+\d+/\d+" s))

(defn regex-positions [re s]
  (loop [
          m (re-matcher re s)
          res #{}
          ]
          (if (.find m)
            (recur m (union res #{(.end m)}))
            (set res))))

(defn cleanse-signature [s]
  (str/trim
    (str/replace s #"[\.,;)]+$" "")))

(defn slash-token-good? [i]
  (and
    (common/not-nil? i)
    (< i 5)))

(defn extract-signature-from-position [s start end]
  (let [
          tokens (str/split (subs s start end) #"\s")
          tokens-with-slash-indices
            (common/indices
              #(common/substring? "/" %)
              tokens)
          first-token-with-slash-index
            (if (nil? tokens-with-slash-indices)
              nil
              (first tokens-with-slash-indices))
    ]
  (if (slash-token-good? first-token-with-slash-index)
      (let [
              last-token-with-slash-index
                (if
                  (and
                    (< first-token-with-slash-index (dec (count tokens)))
                    (common/substring? "/"
                      (nth tokens (inc first-token-with-slash-index))))
                  (inc first-token-with-slash-index)
                  first-token-with-slash-index)
              token (nth tokens last-token-with-slash-index)
              signature-end-position
                (+
                  (str/.indexOf
                    (subs s start end)
                    token)
                  (count token))
              signature (subs s start (+ signature-end-position start))
          ]
          signature)
  "")))

(defn extract-signatures-universal [s]
  (let [
          pos
            (concat
              (regex-positions #"sygn\.\s*(akt)?:?" s)
              (regex-positions #"sygnaturą\s*(akt)?:?" s)
              (regex-positions #"sygnaturze\s*(akt)?:?" s)
              (regex-positions #"sygnaturach\s*(akt)?:?" s))
          pos
            (sort
              (concat pos [(count s)]))
        ]
        (set
          (for [i (range (dec (count pos)))]
            (extract-signature-from-position
              s (nth pos i) (nth pos (inc i)))))))

(defn extract-all-signatures [s]
  (let [
          all
            (union
              (extract-signatures-universal s)
              (extract-signatures-nsa s)
              (extract-signatures-sn s)
              (extract-signatures-tk s)
              (extract-signatures-osp s)
              (extract-signatures-kio-uzp s)
              (extract-signatures-kio-space s)
              (extract-signatures-kio-no-space s))
          clean (map #(cleanse-signature %) all)
          ]
          (set (remove #(= "" %) clean))))

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
          to-write (concat [file-name] [(count signatures)] signatures)
          csv (common/seq-to-csv to-write) 
        ]
        (spit output-file-path csv)))
 