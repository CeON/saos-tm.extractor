(ns saos-tm.extractor.judgment-links
  "Module contains algorithm for extraction of references to Polish case law.
  These references appear in form of case numbers."
  (:require
   [saos-tm.extractor.common :as common]
   [clojure.string :as str]
   [clojure.set :as set])
  (:import java.io.File)
  (:gen-class))

(def ^:private osp-regex
  #"[IVXLCDM]+[\s\.]+[0-9]*[\s\.]*[a-zA-Z]*-?[a-zA-Z]*[\s\.]+\d+/\d+")

(def ^:private kio-regex #"KIO\s*\d+\s*/\s*\d+")

(def ^:private kio-uzp-regex #"KIO\s*/\s*UZP\s+\d+\s*(/\s*\d+)?")

(def ^:private kio-uzp-no-space-regex #"KIO\s*/\s*UZP\s*/\s*\d+\s*/\s*\d+")

(def ^:private kio-uzp-zo-regex #"UZP\s*/\s*ZO\s*/\s*\d+-\d+\s*/\s*\d+")

(def ^:private tk-regex-str
  "(Co|K|Kp|U|P|SK|Kpt|Pp|M|S|T|Tp|Ts|Tw|Twn|Kw|Uw|W)\\.?\\s+\\d+/\\d+")

(def ^:private tk-extraction-regex
  (re-pattern (str "[^a-zA-Z0-9]" tk-regex-str)))

(def ^:private sn-regex #"SNO\s+\d+/\d+")

(def ^:private nsa-regex
  (re-pattern (str "[IVXLCDM]+\\s+[a-zA-Z]+/[a-zA-Z]+\\s+\\d+/\\d+|"
                   "[a-zA-Z]+/[a-zA-Z]+\\s+\\d+/\\d+|"
                   "OPS+\\s+\\d+/\\d+")))

(def ^:private case-nmb-indicators-regex
  #"(?i)(sygn\.*|sygnatur[^\s]*)\s*(akt)?:?")

(def ^:private not-substring? (complement common/substring?))

(defn ^:private extract [re s fnc]
  (set
   (map
    #(str/replace (fnc %) common/system-newline " ")
    (re-seq re s))))

(defn ^:private extract-case-nmbs-osp [s]
  (extract osp-regex s identity))

(defn ^:private extract-case-nmbs-kio [s]
  (extract kio-regex s identity))

(defn ^:private extract-case-nmbs-kio-uzp [s]
  (extract kio-uzp-regex s #(if (string? %) % (first %))))

(defn ^:private extract-case-nmbs-kio-uzp-zo-no-space [s]
  (extract kio-uzp-no-space-regex s identity))

(defn ^:private extract-case-nmbs-kio-uzp-zo [s]
  (extract kio-uzp-zo-regex s identity))

(defn ^:private choose-string [element]
  (if (string? element)
    element
    (first element)))

(defn ^:private extract-tk [s]
  (let [
        string-or-groups (re-seq tk-extraction-regex s)
        matches (map choose-string string-or-groups)
        ]
    matches))

(defn ^:private extract-case-nmbs-tk [s]
  (set
   (map
    #(str/replace % common/system-newline " ")
    (extract-tk s))))

(defn ^:private extract-case-nmbs-sn [s]
  (extract sn-regex s identity))

(defn ^:private extract-case-nmbs-nsa [s]
  (extract nsa-regex s identity))

(defn ^:private cleanse-case-nmb [s]
  (str/trim
   (first
    (str/split
     (common/replace-several s

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

                      #"ogłosz[^\s]*" " "

                      #"\s+" " "
                      #"^[a-ż]+\s" " "
                      #"[^a-żA-Ż0-9]+$" ""
                      #"^[^a-żA-Ż0-9]+" "")
     #"\)|,|\(|;|:„|/\s+[A-TV-Ż]|\s+/[A-TV-Ż]" ))))

(defn ^:private are-subsequent [first-elem second-elem]
  (= (- second-elem first-elem) 1))

(defn ^:private extract-case-nmb-universal [tokens]
  (let [
        tokens-with-slash-indices
          (common/indices
           #(common/substring? "/" %)
           tokens)
        first-index-with-slash (first tokens-with-slash-indices)
        second-index-with-slash (second tokens-with-slash-indices)
        case-nmb-last-token-index
          (if
            (and
             (> (count tokens-with-slash-indices) 1)
             (are-subsequent
              first-index-with-slash
              second-index-with-slash)
             (common/not-matches?
              (nth tokens second-index-with-slash) #"/[\s\S]*"))
            (second tokens-with-slash-indices)
            (first tokens-with-slash-indices))
        case-nmb-tokens
          (take
           (inc case-nmb-last-token-index)
           tokens)
        ]
    (str/join " " case-nmb-tokens)))

(defn ^:private has-slash? [s]
  (common/substring? "/" s))

(defn ^:private extract-case-nmbs-universal
  "Function splits text by case-nmb indicators in `case-nmb-indicators-regex`.
  It takes 5 first tokens in each candidate string.
  Looks for token with slash char.
  If candidate doesn't have such token it is not a case number.
  It takes all tokens up to first with slash or second if they are subsequent."
  [s]
  (let [
        case-nmb-candidates (str/split s case-nmb-indicators-regex)
        ]
    (if (> (count case-nmb-candidates) 1)
      (let [
            case-nmb-candidates
              (map #(str/replace % #"\s+" " ") case-nmb-candidates)
            case-nmb-candidates-in-tokens
              (map
               #(take 5
                      (str/split
                       (str/trim %)
                       #"(?<=[^/])\s+(?=[^/])"))
               case-nmb-candidates)
            having-slash-token-candidates
              (filter
               #(some has-slash? %)
               case-nmb-candidates-in-tokens)
            ]
        (set
         (map
          #(extract-case-nmb-universal %)
          having-slash-token-candidates)))
      nil)))

(defn ^:private extract-cleansed-case-nmbs [extract-fn s]
  (map
    #(cleanse-case-nmb %)
    (extract-fn s)))

(defn ^:private extract-case-nmbs [extract-fncs s]
  (set/union
   (flatten
    (map
     #(extract-cleansed-case-nmbs % s)
     extract-fncs))))

(defn ^:private remove-case-nmbs [case-nmbs-vec s]
  (loop
    [left-case-nmbs case-nmbs-vec
     curr-str s]
    (if
      (empty? left-case-nmbs)
      curr-str
      (recur
       (rest left-case-nmbs)
       (str/replace curr-str (first left-case-nmbs) " ")))))

(defn ^:private one-word-but-not-uzp-kio? [s]
  (and
   (not-substring? " " s)
   (not-substring? "UZP" s)
   (not-substring? "KIO" s)))

(defn extract-judgment-links
  "Extracts links to referenced judgments in the form of case numbers
   present in a given string `s`.

  The result is a set of strings which are case numbers.

  Example:

  `(extract-judgment-links \"wyroki Tw 72/02 i Tw 25/02\")`

  `#{\"Tw 72/02\" \"Tw 25/02\"}`

  It uses two types of functions: universal and specific.

  The first is called `extract-case-nmbs-universal` and looks for
  text like \"sygnatura akt\" that indicates appearance of case number in text.

  Example of function that represent the second type is `extract-case-nmbs-sn`.
  The functions of second type find case numbers in specific formats.
  "
  [s]
  (let [
        preprocessed (common/preprocess s)

        case-nmbs-universal
          (extract-case-nmbs
           [extract-case-nmbs-universal]
           preprocessed)
        case-nmbs-universal-set (set case-nmbs-universal)
        without-universal-case-nmbs
          (remove-case-nmbs case-nmbs-universal-set preprocessed)

        case-nmbs-osp-kio-space
          (extract-case-nmbs
           [extract-case-nmbs-osp
            extract-case-nmbs-kio
            extract-case-nmbs-kio-uzp]
           without-universal-case-nmbs)
        case-nmbs-osp-kio-space-set (set case-nmbs-osp-kio-space)
        without-universal-osp-kio-space-case-nmbs
          (remove-case-nmbs
           case-nmbs-osp-kio-space-set without-universal-case-nmbs)

        all-case-nmbs
          (set/union
           case-nmbs-universal-set
           case-nmbs-osp-kio-space-set
           (extract-case-nmbs
            [extract-case-nmbs-nsa
             extract-case-nmbs-sn
             extract-case-nmbs-tk
             extract-case-nmbs-kio-uzp-zo
             extract-case-nmbs-kio-uzp-zo-no-space]
            without-universal-osp-kio-space-case-nmbs))

        result
          (remove
           #(or
             (= "" %)
             (one-word-but-not-uzp-kio? %)
             (= "KIO/UZP" %))
           all-case-nmbs)
        ]
    (set result)))
