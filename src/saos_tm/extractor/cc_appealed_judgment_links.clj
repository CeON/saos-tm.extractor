(ns saos-tm.extractor.cc-appealed-judgment-links
  (:require
   [ clojure.string :as str ]
   [ langlab.core.parsers :refer :all ]
   [saos-tm.extractor.common :refer :all])
  (:import java.io.File)
  (:gen-class))

(def osp-regex
  (str "[IVXLCDM]+[\\s\\.]*[0-9]*[\\s\\.]*"
    "[a-zA-Z]*-?[a-zA-Z]*[\\s\\.]+\\d+\\s*/\\s*\\d+(/[A-Z]+)?(\\s*upr\\.?)?"))

(defn cleanse-appeal-str [s]
  (let [
        without-html-tags (remove-html-tags-other-than-span s)
        without-newlines
          (str/replace without-html-tags (re-pattern system-newline) " ")
        ]
    without-newlines))

(defn find-from-to-first [s find-regex-func from to case-sensitivity]
  (find-regex-func
   (re-pattern
    (str case-sensitivity from "((?!" to ")[\\s\\S])*" to))
   s))

(defn find-from-to-first-case-ins [s find-regex-func from to]
  (find-from-to-first s find-regex-func from to "(?i)"))

(defn find-from-to-first-case-sen [s find-regex-func from to]
  (find-from-to-first s find-regex-func from to ""))

(defn split-appeal-str [s]
  (let [
        s (cleanse-appeal-str s)
        appeal-type (re-find #"apelacj[^\s]*|zażalen[^\s]*" s)
        appeal-str (str/replace s (re-pattern appeal-type) "")
        parts (str/split appeal-str #"[\s\>]na[\s\<]|[\s\>]od[\s\<]")
        appellant (first parts)
        judgment-type
          (first
           (find-from-to-first-case-ins
            (second parts)
            re-find
            "" #"postanow[^\s]*|wyrok[^\s]*\s*(wstępn[^\s]*)?|zarządz[^\s]*"))
        appeal-parts
          (if
            (nil? judgment-type)
            (take 6 (repeat ""))
            (let [
                  remainder (str/replace (second parts) judgment-type "")
                  parts
                    (str/split
                     remainder
                     #"z\s*dnia|o? ?\(?sygn?(atura)?\s*(akt[\.:]?)?|w sprawie")
                  date-index 1
                  parts
                    (if (> (count parts) 1)
                      (assoc
                        parts
                        date-index
                        (first
                         (str/split (second parts) #"(?<=r\.)|(?<=roku)"))))
                  parts (filter #(not-matches? % #"\s*") parts)
                  ]
              (concat [appeal-type appellant judgment-type] parts)))
              ]
            appeal-parts))

(defn post-process-appellant [s]
  (if (substring? "przez" s)
    (second (str/split s #"przez"))
    s))

(defn cleanse-appeal [s]
  (let [
        s (if (string? s) s (first s))
        result
          (when (not-nil? s)
            (str/trim
             (replace-several s
                              #"akt[:\.]?" ""

                              #"^\s*\.+" ""
                              #"^\s*:" ""

                              #",\s*$" ""
                              #"-\s*$" ""

                              #"roku\." "roku"

                              #"\s+" " ")))
        ]
    result))

(defn map-appeal-parts [coll]
  (let [
        appeal-parts
          (conj
           (drop 2 coll)
           (post-process-appellant (second coll))
           (first coll))
        appeal-parts-trimmed
          (map #(cleanse-appeal %) appeal-parts)
        ]
  (zipmap [:appeal-type :appellant :judgment-type :court :date :signature]
          appeal-parts-trimmed)))

(defn extract-non-complaint [s]
  (let [
        appeal-str-cleansed (cleanse-appeal-str s)
        appeal-parts (split-appeal-str appeal-str-cleansed)
        appeal-parts-map (map-appeal-parts appeal-parts)
        ]
    appeal-parts-map))

(defn extract-appeal-or-grievance [s]
  (let [
         appeal-match-groups
           (map
            #(first
              (find-from-to-first-case-ins
               s regexes-with-starts-ends-sorted % osp-regex))
             ["(?<=skutek)\\s+apelacji" "(?<=z powodu)\\s+apelacji"
              "(?<=skutek)\\s+zażalenia"
              "(?<=z powodu)\\s+zażalenia" "zażalenia\\s+wniesionego"
              "(?<=w związku z)\\s+zażaleniem" "zażalenia\\s+wnioskodawc"
              "zażalenia pow" "(?<=w przedmiocie) zażalenia"
              "zażalenia pokrzywdz" "zażalenia str"
              "zażalenia oskarżon"])
        appeal-match-groups (sort (remove #(nil? %) appeal-match-groups))
        appeal-match-str (second (first appeal-match-groups))
        result
          (cond
           (nil? appeal-match-str)
           nil
           :else
           (extract-non-complaint appeal-match-str))
        ]
    result))

(defn identify-complaint [s]
  (let [
        s (cleanse-appeal-str s)
        part
          (first
           (find-from-to-first-case-sen s re-find  "UZASADNIENIE" osp-regex))
        signature
          (first (re-find (re-pattern osp-regex) part))
        date-str
          (first
           (find-from-to-first-case-ins part re-find "dni[a|u]" "r\\."))
        date
          (when (not-nil? date-str)
            (second (str/split date-str #"\s" 2)))
        appeal-type (re-find #"skar[^\s]*" part)
        parts (drop 1 (str/split part #" że | na | w sprawie"))
        parts
          (if (= (count parts) 2)
            [(str/split (first parts) #"\s(?=[A-Z])" 2)
             (first (str/split (second parts) #"o? sygn"))])
        court
          (when (not-nil? parts)
            (if (= (count (first parts)) 2)
              (second (first parts))
              (second parts)))
        reason
          (when (not-nil? parts)
            (first (first parts)))
        appellant (first (str/split part #"[^\s]* skar"))
        appellant (str/replace appellant #"UZASADNIENIE" "")
        appellant
          (if (substring? "r." appellant)
            (second (str/split appellant #"r\."))
            appellant)
        ]
     (zipmap [:appeal-type :appellant :judgment-type :court :date :signature]
             (map #(when (not-nil? %) (str/trim %))
                  [appeal-type appellant reason court date signature]))))

(defn extract-complaint [s]
  (let [
        appeal-match-groups-complaint
          (map #(first (find-from-to-first-case-ins s re-find  % osp-regex))
             ["na skutek skarg" "z powodu skarg" "sprawy ze skarg"
              "przedmiocie skarg" "skargi? wniesion"
              "w związku ze skarg"])

        appeal-match-str-complaint
          (find-first #(not-nil? %) appeal-match-groups-complaint)

        result
          (cond
           (nil? appeal-match-str-complaint)
           nil
           :else
           (identify-complaint s))
        ]
    result))
