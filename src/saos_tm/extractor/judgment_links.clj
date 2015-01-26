(ns saos-tm.extractor.judgment-links
  (:require
   [ saos-tm.extractor.common :refer :all ]
   [ clojure.string :as str ]
   [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
   [ clojure.set :refer :all]
   )
  (:import java.io.File)
  (:gen-class))

(defn extract [re s]
  (let [
        signatures
          (set
           (map
            #(str/replace % system-newline " ")
            (re-seq re s)))
        ]
    signatures))

(def osp-regex
  #"[IVXLCDM]+[\s\.]+[0-9]*[\s\.]*[a-zA-Z]*-?[a-zA-Z]*[\s\.]+\d+/\d+")

(defn is-osp-signature? [s]
  (matches? s osp-regex))

(defn extract-signatures-osp [s]
  (extract osp-regex s))

(def kio-no-space-regex #"KIO/UZP/\d+/\d+")

(defn extract-signatures-kio-no-space [s]
  (extract kio-no-space-regex s))

(def kio-space-regex #"KIO/UZP\s+\d+/\d+")

(defn extract-signatures-kio-space [s]
  (extract kio-space-regex s))

(def kio-uzp-regex #"UZP/ZO/\d+-\d+/\d+")

(defn extract-signatures-kio-uzp [s]
  (extract kio-uzp-regex s))

(defn is-kio-signature? [s]
  (or
   (matches? s kio-no-space-regex)
   (matches? s kio-space-regex)
   (matches? s kio-uzp-regex)))

(defn not-tk? [s]
  (or
   (substring? "nr" s)
   (substring? "Nr" s)
   (substring? "OSNC" s)
   (= (last s) "/")))

(def tk-regex-str
  "(Co|K|Kp|U|P|SK|Kpt|Pp|M|S|Tp|Ts|Tw|Twn|Kw|Uw|W)\\s+\\d+/\\d+")
(def tk-extraction-regex
  (re-pattern (str "[^a-zA-Z0-9]" tk-regex-str)))
(def tk-check-regex
  (re-pattern  tk-regex-str))

(defn is-tk-signature? [s]
  (matches? s tk-check-regex))

(defn choose-string [element]
  (if (string? element)
    element
    (first element)))

(defn extract-tk [s]
  (let [
        string-or-groups (re-seq tk-extraction-regex s)
        matches
          (map
           #(choose-string %)
           string-or-groups)
        ]
    matches))

(defn extract-signatures-tk [s]
  (set
   (map
    #(str/replace % system-newline " ")
    (extract-tk s))))

(def sn-regex #"[a-zA-Z]+\s+[IVXLCDM]+-\d+-\d+/\d+")
(def sn-regex-1 #"[^IVXLCDM]\s[A-Z]+\s+\d+/\d+")

(defn is-sn-signature? [s]
  (matches? s sn-regex))

(defn is-sn-or-osp-signature? [s]
  (or
   (matches? s sn-regex)
   (matches? s osp-regex)))

(defn extract-signatures-sn [s]
  (extract sn-regex s))

(defn extract-signatures-sn-1 [s]
  (map
   #(apply str (drop 1 %))
   (extract sn-regex-1 s)))

(def nsa-regex #"[IVXLCDM]+\s+[a-zA-Z]+/[a-zA-Z]+\s+\d+/\d+")
(def nsa-regex-1 #"[a-zA-Z]+/[a-zA-Z]+\s+\d+/\d+")

(defn is-nsa-signature? [s]
  (or (matches? s nsa-regex) (matches? s nsa-regex-1)))

(defn extract-signatures-nsa [s]
  (extract nsa-regex s))

(defn extract-signatures-nsa-1 [s]
  (extract nsa-regex-1 s))

(defn cleanse-signature [s]
  (str/trim
   (replace-several s
                    #"[^a-zA-Z0-9]+$" ""
                    #"^[^a-zA-Z0-9]+" "")))

(defn are-subsequent [first-elem second-elem]
  (= (- second-elem first-elem) 1))

(defn extract-signature-universal [tokens]
  (let [
        tokens-with-slash-indices
          (indices
           #(substring? "/" %)
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
  (substring? "/" s))

(def signature-regex #"(?i)sygn[^\s]*\s+(akt)?:?")

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

(defn remove-newlines [s]
  (let [
        without-double-slash-newlines (str/replace s #"\\n" " ")
        without-newlines
          (str/replace without-double-slash-newlines
                       (re-pattern system-newline) " ")
        ]
    without-newlines))

(defn extract-all-signatures [s]
  (let [
        all
          (union
           (extract-signatures-universal s)
           (extract-signatures-nsa s)
           (extract-signatures-nsa-1 s)
           (extract-signatures-sn s)
           (extract-signatures-sn-1 s)
           (extract-signatures-tk s)
           (extract-signatures-osp s)
           (extract-signatures-kio-uzp s)
           (extract-signatures-kio-space s)
           (extract-signatures-kio-no-space s))
        clean (map #(cleanse-signature %) all)
        ]
    (set
     (remove
      #(or
        (= "" %)
        (and
         ((complement substring?) " " %)
         ((complement substring?) "UZP" %)
         ((complement substring?) "KIO" %)))
        clean))))

(defn dehtmlise [s]
  (let [
        s (remove-all-html-tags s)
        s (remove-newlines s)
        ]
    s))

(defn extract-signatures-from-file
  [input-file-path output-file-path]
  (time
   (let [
         input-txt (slurp input-file-path)
         signatures (extract-all-signatures input-txt)
         file-name
           (last
            (str/split
             input-file-path
             (re-pattern (str File/separatorChar))))
         osp-or-sn-signatures-count
           (count
            (filter
             #(is-sn-or-osp-signature? %)
             signatures))
         kio-signatures-count
           (count
            (filter
             #(is-kio-signature? %)
             signatures))
         tk-signatures-count
           (count
            (filter
             #(is-tk-signature? %)
             signatures))
         nsa-signatures-count
           (count
            (filter
             #(is-nsa-signature? %)
             signatures))
         unknown-signatures-count
           (count
            (remove
             #(or
               (is-sn-or-osp-signature? %)
               (is-kio-signature? %)
               (is-tk-signature? %)
               (is-nsa-signature? %))
             signatures))
         to-write
         (concat
          [file-name]
          [(count signatures)]
          [osp-or-sn-signatures-count]
          [kio-signatures-count]
          [tk-signatures-count]
          [nsa-signatures-count]
          [unknown-signatures-count]
          signatures)
         csv (seq-to-csv to-write)
         ]
     (spit output-file-path csv))))
