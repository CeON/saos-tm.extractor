(ns saos-tm.extractor.common
  (:require
   [clojure.string :as str]
   [clojure.set :refer :all]
   [langlab.core.parsers :refer :all]
   [langlab.core.characters :refer :all])
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

(def not-empty? (complement empty?))

(defn matches? [s re]
  (not-nil? (re-matches re s)))

(def not-matches? (complement matches?))

(def ^String system-newline
  (System/getProperty "line.separator"))

(defn get-art-coords-csv [art-coords]
  (let [
        art-nr (:art art-coords)
        par-nr (:par art-coords)
        ust-nr (:ust art-coords)
        pkt-nr (:pkt art-coords)
        zd-nr  (:zd art-coords)
        lit-nr (:lit art-coords)
        ]
    (apply str
           "\"" art-nr "\"" csv-delimiter
           "\"" par-nr "\"" csv-delimiter
           "\"" ust-nr "\"" csv-delimiter
           "\"" pkt-nr "\"" csv-delimiter
           "\"" zd-nr "\"" csv-delimiter
           "\"" lit-nr "\"" csv-delimiter)))

(defn split-to-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

;; for debugging
(defn print-if-contains [s element]
  (if (substring? element s) (prn s)))
(defn print-that [s]
  (doall (prn) (prn)) (prn s) (doall (prn) (prn)))

(defn insert-at-index [s ch index]
  (str (apply str (take index s)) ch (apply str (drop index s))))

(defn handle-superscript-no-range [s]
  (let [
        length (count s)
        ]
    (if (and (> length 3) (contains-digits-only? s))
      (str (insert-at-index s "(" 3) ")")
      s)))

(defn handle-superscript-range [s]
  (let [
        numbers
          (map handle-superscript-no-range
               (str/split s #"-"))
        ]
    (str/join "-" numbers)))

(defn handle-superscript [s]
  (if (substring? "-" s)
    (handle-superscript-range s)
    (handle-superscript-no-range s)))

(defn extract-nmbs-and-ranges [s]
  (let [
        without-superscript
          (map
           #(str/trim %)
           (str/split s #",|\si\s|oraz"))
        with-superscript
          (map handle-superscript without-superscript)
        ]
    with-superscript))

(defn get-coords-names [is-art is-par is-ust is-pkt is-zd is-lit]
  (filter #(not= "" %)
    [(if is-art "art" "")
     (if is-par "par" "")
     (if is-ust "ust" "")
     (if is-pkt "pkt" "")
     (if is-zd "zd." "")
     (if is-lit "lit." "")]))

(defn zero-if-empty [item]
  (if (empty? item)
    ["0"]
    item))

(defn convert-ranges-to-single-records [record]
  (map
   #(zero-if-empty (record %))
   ["art" "par" "ust" "pkt" "zd." "lit."]))

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
      more (cartesian-product (rest colls))]
      (cons x more))))

(defn remove-trailing-conjunction [s]
  (str/replace s #"\si$|\sz$|\soraz$" ""))

(defn extract-coords [s]
  (let [
        numbers
          (map extract-nmbs-and-ranges
               (drop 1
                     (str/split
                      s
                      #"Art\.|art\.|§|ust\.|ust|pkt|zd\.|zd|lit\.|lit")))
        coords-names
          (get-coords-names
           (or (substring? "art." s) (substring? "Art." s))
           (substring? "§" s)
           (substring? "ust" s)
           (substring? "pkt" s)
           (substring? "zd" s)
           (substring? "lit" s))
        full-coords
           (convert-ranges-to-single-records
            (zipmap coords-names numbers))
        ]
    (cartesian-product full-coords)))

(defn insert-art [s]
  (if (matches? s #"^(A|a)rt[\S\s]*")
    s
    (str "art. " s)))

(defn extract-art-coords-with-multiple-art-numbers [art-part other-part]
  (let [
        art-parts (str/split art-part #",|\si\s|\soraz\s|\slub\s")
        last-part (str/join "" [(last art-parts) other-part])
        to-extract-coll (conj (drop-last art-parts) last-part)
        to-extract-with-art-coll (map insert-art to-extract-coll)
        ]
    (mapcat extract-coords to-extract-with-art-coll)))

(defn extract-coords-for-single-art [s]
  (let [
        parts
          (split*
           (str/replace s #",$" "")
           #"§|ust\.|ust|pkt|zd\.|zd|lit\.|lit")
        art-part (first parts)
        other-part (str/join "" (rest parts))
        result
          (if (or
               (substring? "," art-part)
               (substring? " i " art-part)
               (substring? " oraz " art-part)
               (substring? " lub " art-part))
            (extract-art-coords-with-multiple-art-numbers art-part other-part)
            (extract-coords (str/replace s #";$" "")))
        ]
    result))

(defn replace-several [content & replacements]
  (let [
        replacement-list (partition 2 replacements)
        ]
    (reduce
     #(apply str/replace %1 %2)
     content
     replacement-list)))

(defn extract-art-coords [s]
  (let [
        trimmed (str/trim s)
        separate-art-coords
          (if (substring? "art." trimmed)
            (map
             #(apply str "art." %)
             (drop 1
                   (str/split trimmed #"art\.")))
            [trimmed])
        separate-art-coords-trimmed
          (map str/trim separate-art-coords)
        without-conjunctions
          (map remove-trailing-conjunction separate-art-coords-trimmed)
        ]
    (mapcat extract-coords-for-single-art without-conjunctions)))

(defn get-year-from-act-name [s]
  (let [
        pattern (last (re-seq #"\d+\s*r" s))
        ]
        (when
          (not-empty pattern)
          (apply str
            (re-seq #"\d+" pattern)))))

(defn get-year-of-law-act [s]
  (let [
        to-first-closing-parenthesis
          (re-find #"[^\(]*\([^\)]*\)" s)
;;         _ (prn to-first-closing-parenthesis)
        before-law-change-indication
          (if (nil? to-first-closing-parenthesis)
            s
            (first (str/split to-first-closing-parenthesis #"zm\." )))
        txt
          (if (nil? before-law-change-indication)
            s
            before-law-change-indication)
        pattern
          (re-find
           #"Dz\.\s*U\.\s*z?\s*\d+\s*r|Dz\.\s*U\.\s*\d{4}"
           txt)
        year
          (if
            (not-empty pattern)
            (apply str
                   (re-seq #"[\d]+" pattern))
            (get-year-from-act-name
             (first
              (str/split txt #"Dz\."))))
;;         _ (prn "===============")
;;         _ (prn year)
;;         _ (prn "===============")
        ]
    year))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn seq-to-csv [coll]
  (let [
          with-delim-at-the-end
            (str/join ""
              (map
                #(apply str "\"" % "\"" csv-delimiter)
                coll))
          with-newline-at-the-end
            (str
              (apply str (drop-last with-delim-at-the-end))
              system-newline)
            ]
            with-newline-at-the-end))

(defn get-measure [true-positives-count elements-count]
  (if
    (= elements-count 0)
    nil
    (float (/ true-positives-count elements-count))))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn dexmlise [s]
  (when (not-nil? s)
    (str/replace s #"\<((?![\<\>])[\s\S])*\>" "")))

(defn find-first [f coll]
  (first (filter f coll)))

(defn get-regex-matches [re s func]
  (loop [match (re-matcher re s)
         result {}]
    (if (.find match)
      (recur match
             (func match result))
      result)))

(defn start-func [match result]
  (assoc result (.start match) (.group match)))

(defn get-regex-matches-with-starts [re s]
  (get-regex-matches re s start-func))

(defn start-end-func [match result]
  (assoc result [(.start match) (.end match)] (.group match)))

(defn get-regex-matches-with-starts-ends [re s]
  (get-regex-matches re s start-end-func))

(defn start-end-map-func [match result]
  (concat
   result
   [{:start (.start match) :end (.end match) :regex (.group match)}]))

(defn get-regex-matches-with-starts-ends-maps [re s]
  (get-regex-matches re s start-end-map-func))

(defn get-sorted [func re s]
  (let [
        regexes (func re s)
        sorted (sort regexes)
        ]
    sorted))

(defn get-regex-matches-with-starts-ends-sorted [re s]
  (get-sorted get-regex-matches-with-starts-ends re s))

(defn get-regex-match [func regex following-text-regex s]
  (get-sorted func
   (re-pattern
    (str regex following-text-regex))
   s))

(defn get-matches [func regexes following-text-regex s]
  (map
   #(get-regex-match func % following-text-regex s)
   regexes))

(defn find-first-not-empty [coll]
  (find-first #(not-empty? %) coll))

(defn get-first-if-groups [match]
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

(defn get-regex-match-case-ins [func regexes following-text-regex s]
  (func
    (map #(str "(?i)" %) regexes) following-text-regex s))

(defn get-regex-match-case-sen [func regexes following-text-regex s]
  (func regexes following-text-regex s))

(defn get-closest-regex-match-case-ins [regexes following-text-regex s]
  (get-regex-match-case-ins
   get-closest-regex-match regexes following-text-regex s))

(defn get-closest-regex-match-case-sen [regexes following-text-regex s]
  (get-regex-match-case-sen
   get-closest-regex-match regexes following-text-regex s))

(defn remove-all-html-tags [s]
  (str/replace s
               (re-pattern (str "<[^>]*>")) " "))

(defn remove-html-tags-other-than [tag-name s]
  (str/replace s
               (re-pattern (str "<(?!/?" tag-name ")((?!>)[\\s\\S])*>")) " "))

(defn remove-html-tags-other-than-span [s]
  (remove-html-tags-other-than "span" s))

(defn conv-html-to-text [ ^String s]
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

(defn get-csv-for-extracted-link [link signature]
  (let [
        art (:art link)
        act (:act link)
        ]
    (apply str (get-art-coords-csv art)
           "\"" signature "\"" csv-delimiter
           "\"" (:journalYear act) "\"" csv-delimiter
           "\"" (:journalNo act) "\"" csv-delimiter
           "\"" (:journalEntry act) "\"" system-newline)))

(defn remove-hard-spaces [s]
  (str/replace s #"\u00A0" " "))

(defn remove-double-spaces [s]
  (str/replace s #"\s+" " "))

(defn remove-newlines [s]
  (let [
        without-double-slash-newlines (str/replace s #"\\n" " ")
        without-newlines
          (str/replace without-double-slash-newlines
                       (re-pattern system-newline) " ")
        ]
    without-newlines))

(defn unsplit-words-across-lines [s]
  (let [
        without-split-numbers
          (str/replace s
                       (re-pattern (str "(?<=\\d-)" system-newline "(?=\\d)"))
                       "")
        ]
  (str/replace without-split-numbers (str "-" system-newline) "")))

(defn preprocess [s]
  (let [
        without-split-words (unsplit-words-across-lines s)
        without-tags (remove-all-html-tags without-split-words)
        without-hard-spaces (remove-hard-spaces without-tags)
        without-newlines (remove-newlines without-hard-spaces)
        without-double-spaces (remove-double-spaces without-newlines)
        ]
    without-double-spaces))

(def pl-big-diacritics "ĄĆĘŁŃÓŚŻŹ")
(def pl-diacritics (str "ąćęłńóśżź" pl-big-diacritics))
