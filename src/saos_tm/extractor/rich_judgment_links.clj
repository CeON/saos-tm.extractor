(ns saos-tm.extractor.rich-judgment-links
  "Module contains algorithm for extraction of references to Polish case law.
  Extracts more data than judgment-links, because besides case number it
  returns date, court, and judgment type."
  (:require
   [saos-tm.extractor.common :as common]
   [saos-tm.extractor.judgment-links :as judgment-links]
   [clojure.string :as str])
  (:import java.io.File)
  (:gen-class))

(def ^:private date-regex
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

(def ^:private to-word-end "[^\\s]*")

(def ^:private judgment-type-regex-str
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

(def ^:private judgment-type-regex
  (re-pattern judgment-type-regex-str))

(def ^:private nac-regex-str
  (str "KIO|ZA|Zespół\\sArbitrów|Zespołu\\sArbitrów|Izba|Izby|"
       "Krajow" to-word-end "\\sIzb" to-word-end "\\sOdwoławcz" to-word-end))

(def ^:private latin-big-without-roman-digits "A-HJ-UWYZ")

(def ^:private cities-names
  (str
   "(\\s[" latin-big-without-roman-digits common/pl-big-diacritics "]"
   to-word-end ")+"))

(def ^:private non-nac-regex-str
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

(def ^:private court-regex-str
  (str nac-regex-str "|" non-nac-regex-str))

(def ^:private court-regex
  (re-pattern court-regex-str))

(def ^:private starting-or-ending-with-non-letter
  (re-pattern
   (str
    "[^a-zA-Z" common/pl-diacritics "]$|^[^a-zA-Z" common/pl-diacritics "]")))

(defn ^:private national-appeals-chamber? [case-nmb]
  (or
   (common/substring? "UZP/" case-nmb)
   (common/substring? "/UZP" case-nmb)
   (common/substring? "KIO" case-nmb)))

(defn ^:private split-and-take-first [s regex-str]
  (when (common/not-nil? s)
    (first
     (str/split
      s
      (re-pattern regex-str)))))

(defn ^:private extract-no-empties-only-before [before regex]
  (let [
        from-before
          (common/sort-regexes
           (common/get-regex-matches-with-starts-ends-maps regex before)
           :end)
        extracted-element (:match (last from-before))
        ]
    extracted-element))

(defn ^:private extract-only-from-before [before regex]
  (cond
   (empty? before)
   nil
   :else
   (extract-no-empties-only-before before regex)))

(defn ^:private extract-regexes-not-empty [from-before from-after count-before]
  (let [
        before-end-position (:end (last from-before))
        after-start-position (:start (first from-after))
        before-offset (- count-before before-end-position)
        ]
    (if
      (< before-offset after-start-position)
      (:match (last from-before))
      (:match (first from-after)))))

(defn ^:private extract-no-empties [before after regex]
  (let [
        from-before
          (common/sort-regexes
           (common/get-regex-matches-with-starts-ends-maps regex before)
           :end)
        from-after
          (common/sort-regexes
           (common/get-regex-matches-with-starts-ends-maps regex after)
           :start)
        extracted-element
          (cond
           (empty? from-before)
           (:match (first from-after))
           (empty? from-after)
           (:match (last from-before))
           :else
           (extract-regexes-not-empty
            from-before from-after (count before)))
        ]
    extracted-element))

(defn ^:private extract-from-before-and-after [before after regex]
  (cond
   (and (empty? before) (empty? after))
   nil
   (empty? before)
   (:match
    (first
     (common/sort-regexes
      (common/get-regex-matches-with-starts-ends-maps regex after)
      :start)))
   (empty? after)
   (:match
    (last
     (common/sort-regexes
      (common/get-regex-matches-with-starts-ends-maps regex before)
      :end)))
   :else
   (extract-no-empties before after regex)))

(defn ^:private extract-court [parts regex-str]
  (let [
        split-second-part
          (split-and-take-first
           (second parts)
           (str judgment-type-regex-str
                "|\\)\\.\\s(?=[A-Z" common/pl-big-diacritics "])"))
        ]
    (extract-from-before-and-after
     (first parts)
     split-second-part
     (re-pattern regex-str))))

(defn ^:private trim-non-letters [s]
  (str/replace s starting-or-ending-with-non-letter ""))

(defn ^:private postprocess [s]
  (when
    (common/not-nil? s)
    (let [
          before-dot-or-comma
            (first (str/split s #"\.|,"))
          trimmed-non-letters
            (trim-non-letters before-dot-or-comma)
          ]
      trimmed-non-letters)))

(defn ^:private extract-other-data [case-nmb s]
  (let [
        parts
          (str/split s
                     (re-pattern (common/conv-str-to-regex case-nmb)))
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

(defn extract-rich-judgment-links
  "Extracts links to referenced judgments with additional data
  present in a given string `s`.

  The result is a list of maps containing keys:

  * `:caseNumber` - number of referenced case
  * `:judgmentType` - type of referenced judgment
  * `:court` - type of court that lead referenced judgment
  * `:judgmentDate` - date of the judgment

  Example:

  `(extract-rich-judgment-links \"W nieopublikowanym wyroku
  z dnia 2 czerwca 1993 r., SA/Wr 302/93, NSA stwierdził,
  że w trybie art. 101 ust. 1 ustawy o samorządzie terytorialnym\")`

  `({:caseNumber \"SA/Wr 302/93\", :judgmentType \"wyroku\",
  :court \"NSA\", :judgmentDate \"2 czerwca 1993 r.\"})`

  At first it uses `extract-judgment-links` function from `judgment-links`
  module to find numbers of referenced cases. Then it uses `extract-other-data`
  function to return other data regarding case references by means of rules
  and regexes.
  "
  [s]
  (let [
        preprocessed (common/preprocess s)
        case-nmbs (judgment-links/extract-judgment-links s)
        other-judgment-links-data
          (map #(extract-other-data % preprocessed) case-nmbs)
        ]
    (map
     #(assoc %1 :caseNumber %2)
     other-judgment-links-data
     case-nmbs)))
