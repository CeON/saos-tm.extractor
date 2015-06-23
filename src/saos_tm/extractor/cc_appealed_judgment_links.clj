(ns saos-tm.extractor.cc-appealed-judgment-links
  (:require
   [saos-tm.extractor.common :as common]
   [clojure.string :as str]
   [langlab.core.parsers :as langlab-parsers])
  (:import java.io.File)
  (:gen-class))

(def ^:private osp-regex
  (str "[IVXLCDM]+[\\s\\.]*[0-9]*[\\s\\.]*"
    "[a-zA-Z]*-?[a-zA-Z]*[\\s\\.]+\\d+\\s*/\\s*\\d+(/[A-Z]+)?(\\s*upr\\.?)?"))

(def ^:private appeal-indiction-phrases
  ["(?<=skutek)\\s+apelacji" "(?<=z powodu)\\s+apelacji"
   "(?<=skutek)\\s+zażalenia"
   "(?<=z powodu)\\s+zażalenia" "zażalenia\\s+wniesionego"
   "(?<=w związku z)\\s+zażaleniem" "zażalenia\\s+wnioskodawc"
   "zażalenia pow" "(?<=w przedmiocie) zażalenia"
   "zażalenia pokrzywdz" "zażalenia str"
   "zażalenia oskarżon"])

(def ^:private complaint-indiction-phrases
  ["na skutek skarg" "z powodu skarg" "sprawy ze skarg"
   "przedmiocie skarg" "skargi? wniesion" "w związku ze skarg"])

(defn ^:private cleanse-appeal-str [s]
  (let [
        without-html-tags (common/remove-html-tags-other-than-span s)
        without-newlines
          (str/replace without-html-tags
                       (re-pattern common/system-newline) " ")
        ]
    without-newlines))

(defn ^:private find-from-to-first
  [s find-regex-func from to case-sensitivity]
  (find-regex-func
   (re-pattern
    (str case-sensitivity from "((?!" to ")[\\s\\S])*" to))
   s))

(defn ^:private find-from-to-first-case-ins [s find-regex-func from to]
  (find-from-to-first s find-regex-func from to "(?i)"))

(defn ^:private find-from-to-first-case-sen [s find-regex-func from to]
  (find-from-to-first s find-regex-func from to ""))

(defn ^:private split-take-first [s re]
  (first (str/split s re)))

(defn ^:private extract-appeal [parts judgment-type appeal-type appellant]
  (let [
        remainder (str/replace (second parts) judgment-type "")
        remainder-parts
          (str/split
           remainder
           #"z\s*dnia|o? ?\(?sygn?(atura)?\s*(akt[\.:]?)?|w sprawie")
        date-index 1
        with-cleansed-date
          (if (> (count remainder-parts) 1)
            (assoc
              remainder-parts
              date-index
              (split-take-first
               (second remainder-parts) #"(?<=r\.)|(?<=roku)")))
        without-space-only-words
          (filter #(common/not-matches? % #"\s*") with-cleansed-date)
        ]
    (concat
     [appeal-type appellant judgment-type]
     without-space-only-words)))

(defn ^:private split-appeal-str [s]
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
            ""
            #"postanow[^\s]*|wyrok[^\s]*\s*(wstępn[^\s]*)?|zarządz[^\s]*"))
        appeal-parts
          (if (nil? judgment-type)
            (take 6 (repeat ""))
            (extract-appeal parts judgment-type appeal-type appellant))
        ]
    appeal-parts))

(defn ^:private post-process-appellant [s]
  (if (common/substring? "przez" s)
    (second (str/split s #"przez"))
    s))

(defn ^:private cleanse-appeal [s]
  (let [
        s (if (string? s) s (first s))
        result
          (when (common/not-nil? s)
            (str/trim
             (common/replace-several s
                                     #"akt[:\.]?" ""

                                     #"^\s*\.+" ""
                                     #"^\s*:" ""

                                     #",\s*$" ""
                                     #"-\s*$" ""

                                     #"roku\." "roku"

                                     #"\s+" " ")))
        ]
    result))

(defn ^:private map-appeal-parts [coll]
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

(defn ^:private extract-non-complaint [s]
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
               s common/get-regex-matches-with-starts-ends-sorted % osp-regex))
            appeal-indiction-phrases)
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

(defn ^:private identify-complaint [s]
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
          (when (common/not-nil? date-str)
            (second (str/split date-str #"\s" 2)))
        appeal-type (re-find #"skar[^\s]*" part)
        parts (drop 1 (str/split part #" że | na | w sprawie"))
        smaller-parts
          (if (= (count parts) 2)
            [(str/split (first parts) #"\s(?=[A-Z])" 2)
             (first (str/split (second parts) #"o? sygn"))])
        court
          (when (common/not-nil? smaller-parts)
            (if (= (count (first smaller-parts)) 2)
              (second (first smaller-parts))
              (second smaller-parts)))
        reason
          (when (common/not-nil? smaller-parts)
            (first (first smaller-parts)))
        appellant (first (str/split part #"[^\s]* skar"))
        appellant-cleansed (str/replace appellant #"UZASADNIENIE" "")
        appellant-ultimate
          (if (common/substring? "r." appellant-cleansed)
            (second (str/split appellant-cleansed #"r\."))
            appellant-cleansed)
        ]
     (zipmap [:appeal-type :appellant :judgment-type :court :date :signature]
             (map
              #(when (common/not-nil? %) (str/trim %))
              [appeal-type appellant-ultimate reason court date signature]))))

(defn extract-complaint [s]
  (let [
        appeal-match-groups-complaint
          (map #(first (find-from-to-first-case-ins s re-find % osp-regex))
               complaint-indiction-phrases)

        appeal-match-str-complaint
          (common/find-first
           #(common/not-nil? %) appeal-match-groups-complaint)

        result
          (cond
           (nil? appeal-match-str-complaint)
           nil
           :else
           (identify-complaint s))
        ]
    result))
