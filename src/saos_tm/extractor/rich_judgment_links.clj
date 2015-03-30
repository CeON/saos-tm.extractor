(ns saos-tm.extractor.rich-judgment-links
  (:require
   [ clojure.string :as str ]
   [ saos-tm.extractor.judgment-links :refer :all ]
   [ saos-tm.extractor.common :refer :all ])
  (:import java.io.File)
  (:gen-class))

(defn conv-str-to-regex [s]
  (re-pattern
   (str "\\Q" s "\\E")))

(defn sort-regexes [coll end-indicator]
  (sort
   #(compare (end-indicator %1) (end-indicator %2))
   coll))

(defn extract-regexes-not-empty
  [from-before from-after count-before]
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

(defn extract-no-empties-only-before [before regex]
  (let [
        from-before
          (sort-regexes
           (get-regex-matches-with-starts-ends-maps regex before)
           :end)
        extracted-element (:regex (last from-before))
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

(defn extract-only-before [before regex]
  (cond
   (and (empty? before))
   nil
   :else
   (extract-no-empties-only-before before regex)))

(def date-regex
  (re-pattern (str "\\d+\\s("
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

(def judgment-type-regex
  (re-pattern (str "(W|w)yrok[^\\s]*|(P|p)ostanowien[^\\s]*|"
                   "(O|o)rzecze[^\\s]*|(Z|z)arządzen[^\\s]*|"
                   "(U|u)chwal[^\\s]*|(U|u)chwał[^\\s]*|"
                   "(U|u)zasadn[^\\s]*\\suchwał[^\\s]*|"
                   "(U|u)zasadn[^\\s]*\\swyrok[^\\s]*|"
                   "(U|u)zasadn[^\\s]*\\sdo\\suchwał[^\\s]*|"
                   "(U|u)zasadn[^\\s]*\\sdo\\swyrok[^\\s]*"
                   )))

(def court-regex
  (re-pattern
   (str "OTK|NSA|SN|"
        "KIO|ZA|Zespół Arbitrów|Zespołu Arbitrów|Izba|Izby|"
        "Krajow[^\\s]*\\sIzb[^\\s]*\\sOdwoławcz[^\\s]*|"
        "Sąd[^\\s]*\\sNajwyższ[^\\s]*|"
        "Trybunał[^\\s]*\\sKonstyt[^\\s]*|"
        "Prezes[^\\s]*\\sTrybunał[^\\s]*\\sKonstyt[^\\s]*|"
        "Naczeln[^\\s]*\\sSąd[^\\s]*\\sAdm[^\\s]*|"
        "Sąd[^\\s]*\\sApel[^\\s]*\\sw\\s[A-Z][^\\s]*(\\s[A-Z][^\\s]*)?|"
        "Sąd[^\\s]*\\sOkręg[^\\s]*\\sw\\s[A-Z][^\\s]*(\\s[A-Z][^\\s]*)?|"
        "Sąd[^\\s]*\\sRejonow[^\\s]*\\sw\\s[A-Z][^\\s]*(\\s[A-Z][^\\s]*)?|"
        "Wojewódzk[^\\s]*\\sSąd[^\\s]*\\sAdministrac[^\\s]*"
        "\\sw[^\\s]*\\s[A-Z][^\\s]*(\\s[A-Z][^\\s]*)?"
        )))

(def polish-letters-str "ĄąĆćĘęŁłŃńÓóŚśŻżŹź")

(def starting-or-ending-with-non-letter
  (re-pattern
   (str "[^a-zA-Z" polish-letters-str "]$|^[^a-zA-Z" polish-letters-str "]")))

(defn postprocess-court [s]
  (when
    (not-nil? s)
    (let [
          before-dot-or-comma
            (first (str/split s #"\.|,"))
          trimmed-non-letters
            (str/replace
             before-dot-or-comma
             starting-or-ending-with-non-letter
             "")
          ]
      trimmed-non-letters)))

(defn extract-other-data [case-nmb s]
  (let [
        parts (str/split s (re-pattern (conv-str-to-regex case-nmb)))
        date
          (extract-from-before-and-after
           (first parts) (second parts) date-regex)
        court
          (extract-from-before-and-after
           (first parts) (second parts) court-regex)
        judgment-type (extract-only-before (first parts) judgment-type-regex)
        ]
    (zipmap
     [:judgmentDate :court :judgmentType]
     [date
      (postprocess-court court)
      judgment-type])))

(defn extract-ref-judgments [s]
  (let [
        preprocessed (preprocess s)
        case-nmbs (extract-all-signatures preprocessed)
        other-judgment-links-data
          (map #(extract-other-data % preprocessed) case-nmbs)
        ]
    (map
     #(assoc %1 :caseNumber %2)
     other-judgment-links-data
     case-nmbs)))
