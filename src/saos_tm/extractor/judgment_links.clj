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

(defn cleanse-signature [s]
  (str/trim
    (str/replace s #"[\.,;)]+$" "")))

(defn are-subsequent [first-elem second-elem]
  (= (- second-elem first-elem) 1))

(defn extract-signature-universal [tokens]
    (let [
            tokens-with-slash-indices
              (common/indices
                #(common/substring? "/" %)
                tokens)
            signature-last-token-index
              (if
                (and
                  (> (count tokens-with-slash-indices) 1)
                  (are-subsequent
                    (first tokens-with-slash-indices)
                    (second tokens-with-slash-indices)))
                (second tokens-with-slash-indices)
                (first tokens-with-slash-indices))
            signature-tokens
              (take
                (inc signature-last-token-index)
                tokens)
      ]
      (str/join " " signature-tokens)))

(defn has-slash? [s]
  (common/substring? "/" s))

(def signature-regex
  #"sygn\.\s*(akt)?:?|sygnaturą\s*(akt)?:?|sygnaturze\s*(akt)?:?|sygnaturach\s*(akt)?:?")

(defn extract-signatures-universal [s]
  "function splits text by signature indicators"
  "it takes 5 first tokens in each candidate string"
  "looks for token with slash char"
  "if candidate doesn't have such token it is not a signature"
  "it takes all tokens up to first with slash or second if they are subsequent"
  (let [
          signature-candidates (str/split s signature-regex)
    ]
  (if (> (count signature-candidates) 1)
    (let [
            signature-candidates-in-tokens
              (map
                #(take 5
                  (str/split % #"\s"))
                signature-candidates)
            having-slash-token-candidates
              (filter
                #(some has-slash? %)
                signature-candidates-in-tokens)
          ]
          (set
            (map
              #(extract-signature-universal %)
              having-slash-token-candidates)))
    nil)))
    

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
 