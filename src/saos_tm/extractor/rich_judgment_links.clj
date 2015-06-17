(ns saos-tm.extractor.rich-judgment-links
  (:require
   [clojure.string :as str]
   [saos-tm.extractor.judgment-links :refer :all]
   [saos-tm.extractor.common :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn sort-regexes [coll end-indicator]
  (sort
   #(compare (end-indicator %1) (end-indicator %2))
   coll))

(defn national-appeals-chamber? [case-nmb]
  (or
   (substring? "UZP/" case-nmb)
   (substring? "/UZP" case-nmb)
   (substring? "KIO" case-nmb)))

(defn split-and-take-first [s regex-str]
  (when (not-nil? s)
    (first
     (str/split
      s
      (re-pattern regex-str)))))

(defn extract-no-empties-only-before [before regex]
  (let [
        from-before
          (sort-regexes
           (get-regex-matches-with-starts-ends-maps regex before)
           :end)
        extracted-element (:regex (last from-before))
        ]
    extracted-element))

(defn extract-only-from-before [before regex]
  (cond
   (empty? before)
   nil
   :else
   (extract-no-empties-only-before before regex)))

(defn extract-regexes-not-empty [from-before from-after count-before]
  (let [
        before-end-position (:end (last from-before))
        after-start-position (:start (first from-after))
        before-offset (- count-before before-end-position)
        ]
    (if
      (< before-offset after-start-position)
      (:regex (last from-before))
      (:regex (first from-after)))))

(defn extract-no-empties [before after regex]
  (let [
        from-before
          (sort-regexes
           (get-regex-matches-with-starts-ends-maps regex before)
           :end)
        from-after
          (sort-regexes
           (get-regex-matches-with-starts-ends-maps regex after)
           :start)
        extracted-element
          (cond
           (empty? from-before)
           (:regex (first from-after))
           (empty? from-after)
           (:regex (last from-before))
           :else
           (extract-regexes-not-empty
            from-before from-after (count before)))
        ]
    extracted-element))

(defn extract-from-before-and-after [before after regex]
  (cond
   (and (empty? before) (empty? after))
   nil
   (empty? before)
   (:regex
    (first
     (sort-regexes
      (get-regex-matches-with-starts-ends-maps regex after)
      :start)))
   (empty? after)
   (:regex
    (last
     (sort-regexes
      (get-regex-matches-with-starts-ends-maps regex before)
      :end)))
   :else
   (extract-no-empties before after regex)))

(defn conv-str-to-regex [s]
  (re-pattern
   (str "\\Q" s "\\E")))

(def date-regex
  (re-pattern
   (str "\\d+\\s("
        "styczeń|stycznia|"
        "luty|lutego|"
        "marzec|marca|"
        "kwiecień|kwietnia|"
        "maj|maja|"
        "czerwiec|czerwca|"
        "lipiec|lipca|"
        "sierpień|sierpnia|"
        "wrzesień|września|"
        "październik|października|"
        "listopad|listopada|"
        "grudzień|grudnia)"
        "\\s\\d+(\\s?r\\.?)?")))

(def to-word-end "[^\\s]*")

(def judgment-type-regex-str
  (str "(W|w)yrok" to-word-end "|"
       "(P|p)ostanowien" to-word-end "|"
       "(O|o)rzecze" to-word-end "|"
       "(Z|z)arządzen" to-word-end "|"
       "(U|u)chwal" to-word-end "|"
       "(U|u)chwał" to-word-end "|"
       "(U|u)zasadn" to-word-end "\\suchwał" to-word-end "|"
       "(U|u)zasadn" to-word-end "\\swyrok" to-word-end "|"
       "(U|u)zasadn" to-word-end "\\sdo\\suchwał" to-word-end "|"
       "(U|u)zasadn" to-word-end "\\sdo\\swyrok" to-word-end "|"
       "(U|u)zasadn" to-word-end "|"
       "(P|p)ostępowan" to-word-end "|"
       "(S|s)karg" to-word-end "|"
       "(S|s)kardz" to-word-end "|"
       "(P|p)i(s|ś)m" to-word-end ""))

(def judgment-type-regex
  (re-pattern judgment-type-regex-str))

(def nac-regex-str
  (str "KIO|ZA|Zespół\\sArbitrów|Zespołu\\sArbitrów|Izba|Izby|"
       "Krajow" to-word-end "\\sIzb" to-word-end "\\sOdwoławcz" to-word-end))

(def latin-big-without-roman-digits "A-HJ-UWYZ")

(def pl-big-diacritics "ĄĆĘŁŃÓŚŻŹ")
(def pl-diacritics (str "ąćęłńóśżź" pl-big-diacritics))

(def cities-names
  (str
   "(\\s[" latin-big-without-roman-digits pl-big-diacritics "]"
   to-word-end ")+"))

(def non-nac-regex-str
  (str
   "OTK|"
   "NSA|"
   "SN|"
   "Sąd" to-word-end "\\sNajwyższ" to-word-end "|"
   "Trybunał" to-word-end "\\sKonstyt" to-word-end "|"
   "Prezes" to-word-end "\\sTrybunał" to-word-end "\\sKonstyt" to-word-end "|"
   "Naczeln" to-word-end "\\sSąd" to-word-end "\\sAdm" to-word-end "|"
   "Sąd" to-word-end "\\sApel" to-word-end "\\swe?" cities-names "|"
   "Sąd" to-word-end "\\sOkręg" to-word-end "\\swe?" cities-names "|"
   "SO\\swe?" cities-names "|"
   "Sąd" to-word-end "\\sRejonow" to-word-end "\\swe?" cities-names "|"
   "SR\\swe?" cities-names "|"

   "Wojewódzk" to-word-end "\\sSąd" to-word-end
   "\\sAdministrac" to-word-end "\\swe?" cities-names "|"

   "WSA\\swe?" cities-names "|"
   "(P|p)rokurator" to-word-end "\\s(G|g)eneraln" to-word-end ""))

(def court-regex-str
  (str nac-regex-str "|" non-nac-regex-str))

(def court-regex
  (re-pattern court-regex-str))

(defn extract-court [parts regex-str]
  (let [
        split-second-part
          (split-and-take-first
           (second parts)
           (str judgment-type-regex-str
                "|\\)\\.\\s(?=[A-Z" pl-big-diacritics "])"))
        ]
    (extract-from-before-and-after
     (first parts)
     split-second-part
     (re-pattern regex-str))))

(def starting-or-ending-with-non-letter
  (re-pattern
   (str "[^a-zA-Z" pl-diacritics "]$|^[^a-zA-Z" pl-diacritics "]")))

(defn trim-non-letters [s]
  (str/replace s starting-or-ending-with-non-letter ""))

(defn postprocess [s]
  (when
    (not-nil? s)
    (let [
          before-dot-or-comma
            (first (str/split s #"\.|,"))
          trimmed-non-letters
            (trim-non-letters before-dot-or-comma)
          ]
      trimmed-non-letters)))

(defn extract-other-data [case-nmb s]
  (let [
        parts
          (str/split s
                     (re-pattern (conv-str-to-regex case-nmb)))
        date
          (extract-from-before-and-after
           (first parts)
           (split-and-take-first (second parts) (str court-regex-str "|,"))
           date-regex)
        court
          (if (national-appeals-chamber? case-nmb)
            (extract-court parts nac-regex-str)
            (extract-court parts non-nac-regex-str))
        judgment-type
          (extract-only-from-before (first parts) judgment-type-regex)
        ]
    (zipmap
     [:judgmentDate :court :judgmentType]
     [date (postprocess court) (postprocess judgment-type)])))

(defn extract-ref-judgments [s]
  (let [
        preprocessed (preprocess s)
        case-nmbs (extract-all-signatures s)
        other-judgment-links-data
          (map #(extract-other-data % preprocessed) case-nmbs)
        ]
    (map
     #(assoc %1 :caseNumber %2)
     other-judgment-links-data
     case-nmbs)))
