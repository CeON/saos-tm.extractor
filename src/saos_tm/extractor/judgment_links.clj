(ns saos-tm.extractor.judgment-links
  (:require
   [saos-tm.extractor.common :refer :all]
   [clojure.string :as str]
   [langlab.core.parsers :refer [lg-split-tokens-bi]]
   [clojure.set :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn extract [re s fnc]
  (set
   (map
    #(str/replace (fnc %) system-newline " ")
    (re-seq re s))))

(def osp-regex
  #"[IVXLCDM]+[\s\.]+[0-9]*[\s\.]*[a-zA-Z]*-?[a-zA-Z]*[\s\.]+\d+/\d+")

(defn is-osp-signature? [s]
  (matches? s osp-regex))

(defn extract-signatures-osp [s]
  (extract osp-regex s identity))

(def kio-regex #"KIO\s*\d+\s*/\s*\d+")

(defn extract-signatures-kio [s]
  (extract kio-regex s identity))

(def kio-uzp-regex #"KIO\s*/\s*UZP\s+\d+\s*(/\s*\d+)?")

(defn extract-signatures-kio-uzp [s]
  (extract kio-uzp-regex s #(if (string? %) % (first %))))

(def kio-uzp-no-space-regex #"KIO\s*/\s*UZP\s*/\s*\d+\s*/\s*\d+")

(defn extract-signatures-kio-uzp-zo-no-space [s]
  (extract kio-uzp-no-space-regex s identity))

(def kio-uzp-zo-regex #"UZP\s*/\s*ZO\s*/\s*\d+-\d+\s*/\s*\d+")

(defn extract-signatures-kio-uzp-zo [s]
  (extract kio-uzp-zo-regex s identity))

(defn is-kio-signature? [s]
  (or
   (matches? s kio-regex)
   (matches? s kio-uzp-regex)
   (matches? s kio-uzp-no-space-regex)
   (matches? s kio-uzp-zo-regex)))

(def tk-regex-str
  "(Co|K|Kp|U|P|SK|Kpt|Pp|M|S|Tp|Ts|Tw|Twn|Kw|Uw|W)\\.?\\s+\\d+/\\d+")
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

(def sn-regex
  #"[a-zA-Z]+\s+[IVXLCDM]+-\d+-\d+/\d+|SNO\s+\d+/\d+")

(defn is-sn-signature? [s]
  (matches? s sn-regex))

(defn extract-signatures-sn [s]
  (extract sn-regex s identity))

(defn is-sn-or-osp-signature? [s]
  (or
   (matches? s sn-regex)
   (matches? s osp-regex)))

(def nsa-regex
  (re-pattern (str "[IVXLCDM]+\\s+[a-zA-Z]+/[a-zA-Z]+\\s+\\d+/\\d+|"
                   "[a-zA-Z]+/[a-zA-Z]+\\s+\\d+/\\d+|"
                   "OPS+\\s+\\d+/\\d+")))

(defn is-nsa-signature? [s]
  (matches? s nsa-regex))

(defn extract-signatures-nsa [s]
  (extract nsa-regex s identity))

(defn cleanse-signature [s]
  (str/trim
   (first
    (str/split
     (replace-several s

                      #"^[0-9]+\." " "

                      #"\s[a-ż]+\s" " "
                      #"(?i)sąd[^\s]*" ""
                      #"(?i)okręg[^\s]*" " "
                      #"(?i)rejon[^\s]*" " "
                      #"(?i)apel[^\s]*" " "
                      #"(?i)uzasadnienie" " "
                      #"(?i)postanowienie" " "
                      #"(?i)wyrok" " "
                      #"(?i)najwyż[^\s]*" " "
                      #"/[a-ż]+" " "
                      #"[a-ż]+:" " "

                      #"\s+" " "
                      #"^[a-ż]+\s" " "
                      #"[^a-żA-Ż0-9]+$" ""
                      #"^[^a-żA-Ż0-9]+" "")
     #"\)|,|\(|;|:„|/\s+[A-TV-Ż]|\s+/[A-TV-Ż]" ))))

(defn are-subsequent [first-elem second-elem]
  (= (- second-elem first-elem) 1))

(defn extract-signature-universal [tokens]
  (let [
        tokens-with-slash-indices
          (indices
           #(substring? "/" %)
           tokens)
        first-index-with-slash (first tokens-with-slash-indices)
        second-index-with-slash (second tokens-with-slash-indices)
        signature-last-token-index
          (if
            (and
             (> (count tokens-with-slash-indices) 1)
             (are-subsequent
              first-index-with-slash
              second-index-with-slash)
             (not-matches? (nth tokens second-index-with-slash) #"/[\s\S]*"))
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

(def signature-regex #"(?i)(sygn\.*|sygnatur[^\s]*)\s*(akt)?:?")

(defn extract-signatures-universal
  "Function splits text by signature indicators. It takes 5 first tokens
  in each candidate string. Looks for token with slash char. If candidate
  doesn't have such token it is not a signature.
  It takes all tokens up to first with slash or second if they are subsequent."
  [s]
  (let [
        signature-candidates (str/split s signature-regex)
        ]
    (if (> (count signature-candidates) 1)
      (let [
            signature-candidates
              (map #(str/replace % #"\s+" " ") signature-candidates)
            signature-candidates-in-tokens
              (map
               #(take 5
                      (str/split
                       (str/trim %)
                       #"(?<=[^/])\s+(?=[^/])"))
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

(defn extract-cleansed-signatures [extract-fn s]
  (map
    #(cleanse-signature %)
    (extract-fn s)))

(defn extract-signatures [extract-fncs s]
  (union
   (flatten
    (map
     #(extract-cleansed-signatures % s)
     extract-fncs))))

(defn remove-signatures [signatures-vec s]
  (loop
    [left-signatures signatures-vec
     curr-str s]
    (if
      (empty? left-signatures)
      curr-str
      (recur
       (rest left-signatures)
       (clojure.string/replace curr-str (first left-signatures) " ")))))

(def not-substring? (complement substring?))

(defn one-word-but-not-uzp-kio? [s]
  (and
   (not-substring? " " s)
   (not-substring? "UZP" s)
   (not-substring? "KIO" s)))

(defn extract-all-signatures [s]
  (let [
        preprocessed (preprocess s)

        signatures-universal
          (extract-signatures
           [extract-signatures-universal]
           preprocessed)
        signatures-universal-set (set signatures-universal)
        without-universal-signatures
          (remove-signatures
           signatures-universal-set preprocessed)

        signatures-osp-kio-space
          (extract-signatures
           [extract-signatures-osp
            extract-signatures-kio
            extract-signatures-kio-uzp]
           without-universal-signatures)
        signatures-osp-kio-space-set (set signatures-osp-kio-space)
        without-universal-osp-kio-space-signatures
          (remove-signatures
           signatures-osp-kio-space-set without-universal-signatures)

        all-signatures
          (union
           signatures-universal-set
           signatures-osp-kio-space-set
           (extract-signatures
             [extract-signatures-nsa
              extract-signatures-sn
              extract-signatures-tk
              extract-signatures-kio-uzp-zo
              extract-signatures-kio-uzp-zo-no-space]
             without-universal-osp-kio-space-signatures)
;;            (when (not-nil? own-signature) (set [own-signature]))
           )

        result
          (remove
           #(or
             (= "" %)
             (one-word-but-not-uzp-kio? %)
             (= "KIO/UZP" %))
           all-signatures)
        result-set (set result)
        ]
    result-set))
