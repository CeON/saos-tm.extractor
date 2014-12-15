(ns saos-tm.extractor.cc-appealed-judgment-links
  (:require
   [ clojure.string :as str ]
   [ langlab.core.parsers :refer :all ]
   [saos-tm.extractor.common :refer :all])
  (:import java.io.File)
  (:gen-class))

(def osp-regex
  (str "[IVXLCDM]+[\\s\\.]*[0-9]*[\\s\\.]*"
      "[a-zA-Z]*-?[a-zA-Z]*[\\s\\.]+\\d+\\s*/\\s*\\d+(/[A-Z]+)?(\\s*upr\\.)?"))

(defn remove-html-tags-other-than-span [s]
  (str/replace s #"<(?!/?span)((?!>)[\s\S])*>" ""))

(defn cleanse-appeal-str [s]
  (let [
        without-html-tags (remove-html-tags-other-than-span s)
        without-newlines
          (str/replace without-html-tags (re-pattern system-newline) "")
        ]
    without-newlines))

(defn find-from-to-first [s from to case-sensitivity]
  (re-find
   (re-pattern
    (str case-sensitivity from "((?!" to ")[\\s\\S])*" to))
   s))

(defn find-from-to-first-case-ins [s from to]
  (find-from-to-first s from to "(?i)"))

(defn find-from-to-first-case-sen [s from to]
  (find-from-to-first s from to ""))

(defn split-appeal-str-sentence [s]
  (drop 1
        (str/split
         s
         #"apelacji|od\s*wyroku\s*(wstępnego)?|z\s*dnia|\(?sygn(atura)?")))

(defn split-appeal-str-decision [s]
  (let [
        appeal-type (re-find #"apelacj[^\s]*|skarg[^\s]*|zażalen[^\s]*" s)
        appeal-str (second (str/split s (re-pattern appeal-type)))
        parts (split* appeal-str #"postanow[^\s]*|wyrok[^\s]*|zarządz[^\s]*")
        appellant (str/replace (first parts) #"od\s*$|na\s*$" "")
        judgment-type (second parts)
        parts (str/split (nth parts 2) #"z\s*dnia|\(?sygn?(atura)?|w sprawie")
        parts
          (if (string? parts)
            (str/split parts #",")
            parts)
        ]
    (concat [appeal-type appellant judgment-type] parts)))

(defn post-process-appellant [s]
  (if (substring? "przez" s)
    (second (str/split s #"przez"))
    s))

(defn cleanse-appeal [s]
  (str/trim
   (replace-several s
                    #"akt[:\.]?" ""

                    #"^\s*\.+" ""
                    #"^\s*:" ""

                    #",\s*$" ""
                    #"-\s*$" ""

                    #"roku\." "roku"

                    #"\s+" " ")))

(defn map-appeal-parts-sentence [coll]
  (let [
        appeal-parts
          (conj (drop 1 coll) (post-process-appellant (first coll)))
        appeal-parts-trimmed
          (map #(cleanse-appeal %) appeal-parts)
        ]
  (zipmap [:appellant :court :date :signature]
          appeal-parts-trimmed)))

(defn map-appeal-parts-decision [coll]
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

(defn extract-non-complaint [s split-appeal-str-fn map-appeal-parts-fn]
  (let [
        appeal-str-cleansed (cleanse-appeal-str s)
        appeal-parts (split-appeal-str-fn appeal-str-cleansed)
        appeal-parts-map (map-appeal-parts-fn appeal-parts)
        ]
    appeal-parts-map))

(defn extract-signature-sentence [s]
  (let [
        appeal-match-groups
          (concat
           (find-from-to-first-case-ins s "na skutek apelacji" osp-regex)
           (find-from-to-first-case-ins s "z powodu apelacji" osp-regex))
        appeal-match-str (first appeal-match-groups)
        result
          (cond
           (nil? appeal-match-str)
           nil
           :else
           (extract-non-complaint
            appeal-match-str
            split-appeal-str-sentence map-appeal-parts-sentence))
        ]
    result))

(defn extract-complaint [s]
  (let [
        s (cleanse-appeal-str s)
        part (first (find-from-to-first-case-sen s "UZASADNIENIE" osp-regex))
        signature
          (first (re-find (re-pattern osp-regex) part))
        date-str
         (first
          (find-from-to-first-case-ins part "dni[a|u]" "r\\."))
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

(defn extract-signature-decision [s]
  (let [
        appeal-match-groups
         (map #(first (find-from-to-first-case-ins s % osp-regex))
           ["na skutek apelacji" "z powodu apelacji" "na skutek zażalenia"
            "z powodu zażalenia" "zażalenia wniesionego"
            "w związku z zażaleniem" "zażalenia wnioskodawc" "zażalenia pow"])

        appeal-match-groups-complaint
          (map #(first (find-from-to-first-case-ins s % osp-regex))
             ["na skutek skarg" "z powodu skarg" "sprawy ze skarg"
              "przedmiocie skarg" "skargi? wniesion"
              "w związku ze skarg"])

        appeal-match-str (find-first #(not-nil? %) appeal-match-groups)
        appeal-match-str-complaint
          (find-first #(not-nil? %) appeal-match-groups-complaint)

        result
          (cond
           (and (nil? appeal-match-str) (nil? appeal-match-str-complaint))
           nil
           (not-nil? appeal-match-str)
           (extract-non-complaint
            appeal-match-str
            split-appeal-str-decision map-appeal-parts-decision)
           :else
           (extract-complaint s)
           )

        ]
    result))
