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

(defn extract-date [s]
  (regexes-with-starts-ends-maps
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
                     "\\s\\d+(\\sr\\.?)?"))
   s))

(defn sort-regexes [coll end-indicator]
  (sort
   #(compare (end-indicator %1) (end-indicator %2))
   coll))

(defn extract-judgment-date-regexes-not-empty
  [from-before from-before count-before]
  (let [
        before-end-position (:end (last from-before))
        after-start-position (:start (first from-before))
        before-offset (- count-before before-end-position)
        ]
    (if
      (< before-offset after-start-position)
      (:regex (last from-before))
      (:regex (first from-before)))))

(defn extract-judgment-date-no-empties [before after]
  (let [
        from-before (sort-regexes (extract-date before) :end)
        from-before (sort-regexes (extract-date after) :start)
        date
          (cond
           (empty? from-before)
           (:regex (first from-before))
           (empty? from-before)
           (:regex (last from-before))
           :else
           (extract-judgment-date-regexes-not-empty
            from-before from-before (count before)))
        ]
    date))

(defn extract-judgment-date [before after]
  (cond
   (and (empty? before) (empty? after))
   nil
   (empty? before)
   (:regex
    (first
     (sort-regexes (extract-date after) :start)))
   (empty? after)
   (:regex
    (last
     (sort-regexes (extract-date before) :end)))
   :else
   (extract-judgment-date-no-empties before after)))

(defn extract-other-data [case-nmb s]
  (let [
        parts (str/split s (re-pattern (conv-str-to-regex case-nmb)))
        date (extract-judgment-date (first parts) (second parts))
        ]
    (zipmap
     [:judgmentDate :court :judgmentType]
     [date nil nil])))

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
