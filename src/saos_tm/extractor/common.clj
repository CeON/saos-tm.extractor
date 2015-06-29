(ns saos-tm.extractor.common
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.io File]
           [org.apache.commons.io IOUtils]
           [org.apache.tika.parser Parser ParseContext]
           [org.apache.tika.parser.html HtmlParser]
           [org.apache.tika.language LanguageIdentifier]
           [org.apache.tika.metadata Metadata]
           [org.apache.tika Tika]
           [org.apache.tika.sax BodyContentHandler]))

(def csv-delimiter ",")

(def not-nil? (complement nil?))

(def ^:private not-empty? (complement empty?))

(def pl-big-diacritics "ĄĆĘŁŃÓŚŻŹ")

(def pl-diacritics (str "ąćęłńóśżź" pl-big-diacritics))

(def ^String system-newline
  (System/getProperty "line.separator"))

(defn matches? [s re]
  (not-nil? (re-matches re s)))

(def not-matches? (complement matches?))

(defn conv-str-to-regex [s]
  (re-pattern
   (str "\\Q" s "\\E")))

(defn get-file-paths [dir re]
  (let [
          file-paths
            (.listFiles
              (io/file dir))
          file-paths
            (filter #(matches? (str %) re)
                    file-paths)
        ]
    file-paths))

(defn replace-several [content & replacements]
  (let [
        replacement-list (partition 2 replacements)
        ]
    (reduce
     #(apply str/replace %1 %2)
     content
     replacement-list)))

(defn find-first [f coll]
  (first (filter f coll)))

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

(defn indices [pred coll]
   (keep-indexed
    #(when (pred %2) %1)
    coll))

(defn remove-hard-spaces [s]
  (str/replace s #"\u00A0" " "))

(defn remove-double-spaces [s]
  (str/replace s #"\s+" " "))

(defn ^:private remove-newlines [s]
  (let [
        without-double-slash-newlines (str/replace s #"\\n" " ")
        without-newlines
          (str/replace without-double-slash-newlines
                       (re-pattern system-newline) " ")
        ]
    without-newlines))

(defn ^:private unsplit-words-across-lines [s]
  (let [
        without-split-numbers
          (str/replace s
                       (re-pattern (str "(?<=\\d-)" system-newline "(?=\\d)"))
                       "")
        ]
  (str/replace without-split-numbers (str "-" system-newline) "")))

(defn conv-html-to-text
  "Removes all html tags from text in string `s`. Uses Tika html parser
   under the hood."
  [ ^String s]
  (let [
          istream (IOUtils/toInputStream s "UTF-8");
          parser (HtmlParser.)
          context (ParseContext.)
          metadata (Metadata.)
          handler (BodyContentHandler. (.length s))
          ]
      (.set context Parser parser)
      (.parse parser istream handler metadata context)
      (.toString  handler)))

(defn preprocess [s]
  (let [
        without-split-words (unsplit-words-across-lines s)
        without-tags (conv-html-to-text without-split-words)
        without-hard-spaces (remove-hard-spaces without-tags)
        without-newlines (remove-newlines without-hard-spaces)
        without-double-spaces (remove-double-spaces without-newlines)
        ]
    without-double-spaces))

(defn ^:private remove-html-tags-other-than [tag-name s]
  (str/replace s
               (re-pattern (str "<(?!/?" tag-name ")((?!>)[\\s\\S])*>")) " "))

(defn remove-html-tags-other-than-span [s]
  (remove-html-tags-other-than "span" s))

;; for debugging
(defn ^:private print-if-contains [s element]
  (if (substring? element s) (prn s)))
(defn ^:private print-that [s]
  (doall (prn) (prn)) (prn s) (doall (prn) (prn)))

; Utilities for regexes matching

(defn ^:private get-regex-matches [re s func]
  (loop [match (re-matcher re s)
         result {}]
    (if (.find match)
      (recur match
             (func match result))
      result)))

(defn ^:private start-func [match result]
  (assoc result (.start match) (.group match)))

(defn ^:private get-regex-matches-with-starts [re s]
  (get-regex-matches re s start-func))

(defn ^:private start-end-func [match result]
  (assoc result [(.start match) (.end match)] (.group match)))

(defn ^:private get-regex-matches-with-starts-ends [re s]
  (get-regex-matches re s start-end-func))

(defn ^:private start-end-map-func [match result]
  (concat
   result
   [{:start (.start match) :end (.end match) :regex (.group match)}]))

(defn get-regex-matches-with-starts-ends-maps [re s]
  (get-regex-matches re s start-end-map-func))

(defn ^:private get-sorted [func re s]
  (let [
        regexes (func re s)
        sorted (sort regexes)
        ]
    sorted))

(defn get-regex-matches-with-starts-ends-sorted [re s]
  (get-sorted get-regex-matches-with-starts-ends re s))

(defn ^:private get-regex-match [func regex following-text-regex s]
  (get-sorted func
   (re-pattern
    (str regex following-text-regex))
   s))

(defn ^:private get-matches [func regexes following-text-regex s]
  (map
   #(get-regex-match func % following-text-regex s)
   regexes))

(defn ^:private find-first-not-empty [coll]
  (find-first #(not-empty? %) coll))

(defn ^:private get-first-if-groups [match]
  (if (string? match) match (first match)))

(defn get-closest-regex-match [regexes following-text-regex s]
  (let [
        matches-with-positions
          (get-matches
           get-regex-matches-with-starts-ends regexes following-text-regex s)
        matches-with-positions-sorted
          (sort #(compare (first %1) (first %2)) matches-with-positions)
        first-match (find-first-not-empty matches-with-positions-sorted)
        first-match-str (get-first-if-groups first-match)
        ]
    first-match-str))

(defn ^:private get-regex-match-case-ins [func regexes following-text-regex s]
  (func
    (map #(str "(?i)" %) regexes) following-text-regex s))

(defn ^:private get-regex-match-case-sen [func regexes following-text-regex s]
  (func regexes following-text-regex s))

(defn get-closest-regex-match-case-ins [regexes following-text-regex s]
  (get-regex-match-case-ins
   get-closest-regex-match regexes following-text-regex s))

(defn get-closest-regex-match-case-sen [regexes following-text-regex s]
  (get-regex-match-case-sen
   get-closest-regex-match regexes following-text-regex s))
