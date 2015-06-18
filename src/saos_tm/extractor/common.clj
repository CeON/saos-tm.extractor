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

(defn split-to-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

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

(defn preprocess [s]
  (let [
        without-split-words (unsplit-words-across-lines s)
        without-tags (conv-html-to-text without-split-words)
        without-hard-spaces (remove-hard-spaces without-tags)
        without-newlines (remove-newlines without-hard-spaces)
        without-double-spaces (remove-double-spaces without-newlines)
        ]
    without-double-spaces))

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

(defn get-coords-names [is-art is-par is-ust is-pkt is-zd is-lit]
  (filter #(not= "" %)
    [(if is-art "art" "")
     (if is-par "par" "")
     (if is-ust "ust" "")
     (if is-pkt "pkt" "")
     (if is-zd "zd." "")
     (if is-lit "lit." "")]))

(defn zero-if-empty [item]
  (if (empty? item) "0" item))

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

(defn insert-art [s]
  (if (matches? s #"^(A|a)rt[\S\s]*")
    s
    (str "art. " s)))

(defn extract-coords [s]
  (let [
        numbers
          (map handle-superscript
               (map str/trim
                    (drop 1
                          (str/split
                           s
                           #"Art\.?|art\.?|§|ust\.?|pkt\.?|zd\.?|lit\.?"))))
        coords-names
          (get-coords-names
           (or (substring? "art" s) (substring? "Art" s))
           (substring? "§"   s)
           (substring? "ust" s)
           (substring? "pkt" s)
           (substring? "zd"  s)
           (substring? "lit" s))
        full-coords
           (convert-ranges-to-single-records
            (zipmap coords-names numbers))
        ]
    full-coords))

(defn create-coords-list-with-value-at-index [index coords-list-length s]
  (concat
   (take index (repeat "0"))
   [(str/trim s)]
   (take (dec (- coords-list-length index)) (repeat "0"))))

(defn cast-coords-lists [more-important-list less-important-list]
  (let [
        first-non-zero-index
          (count (take-while #(= % "0") less-important-list))
        ]
    (concat
     (take first-non-zero-index more-important-list)
     (drop first-non-zero-index less-important-list))))

(defn extract-coords-lists-for-other-parts [the-part first-part-coords]
  (if
    (or
     (substring? "§"   the-part)
     (substring? "ust" the-part)
     (substring? "pkt" the-part)
     (substring? "zd"  the-part)
     (substring? "lit" the-part))
    (extract-coords the-part)
    (let [
          trailing-zeros-count
            (count
             (take-while #(= % "0")
                         (reverse first-part-coords)))
          index-of-least-important-art-coord
            (dec
             (- (count first-part-coords)
                trailing-zeros-count))
          ]
      (create-coords-list-with-value-at-index
       index-of-least-important-art-coord
       (count first-part-coords)
       the-part))))

(defn convert-to-coords-lists [parts-sep-by-conj]
  (let [
        first-part-coords (extract-coords (first parts-sep-by-conj))
        other-parts-coords
          (if (= (count parts-sep-by-conj) 1)
            nil
            (map #(extract-coords-lists-for-other-parts % first-part-coords)
                 (drop 1 parts-sep-by-conj)))
        other-parts-coords-cast
          (map #(cast-coords-lists first-part-coords %) other-parts-coords)
        ]
    (concat [first-part-coords] other-parts-coords-cast)))

(defn extract-coords-lists [s]
  (let [
        seperated-by-conjunctions (str/split s #",| i | oraz | lub ")
        coords (convert-to-coords-lists seperated-by-conjunctions)
        ]
    coords))

(defn extract-art-coords-with-multiple-art-numbers [art-part other-part]
  (let [
        art-parts (str/split art-part #",|\si\s|\soraz\s|\slub\s")
        last-part (str/join "" [(last art-parts) other-part])
        to-extract-coll (conj (drop-last art-parts) last-part)
        to-extract-with-art-coll (map insert-art to-extract-coll)
        ]
    (mapcat extract-coords-lists to-extract-with-art-coll)))

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
            (extract-coords-lists (str/replace s #";$" "")))
        ]
    result))

(defn cleanse-commas [s]
  (remove-double-spaces
   (replace-several s
                    #"art\.?\s*," "art "
                    #"par\.?\s*," "par "
                    #"ust\.?\s*," "ust "
                    #"pkt\.?\s*," "pkt "
                    #"zd\.?\s*," "zd "
                    #"lit\.?\s*," "lit ")))

(defn extract-art-coords [s]
  (let [
        cleansed-commas (cleanse-commas s)
        trimmed (str/trim cleansed-commas)
        separate-art-coords
          (if (substring? "art" trimmed)
            (map
             #(apply str "art." %)
             (drop 1
                   (str/split trimmed #"art\.?")))
            [trimmed])
        separate-art-coords-trimmed
          (map str/trim separate-art-coords)
        without-conjunctions
          (map remove-trailing-conjunction separate-art-coords-trimmed)
        ]
    (mapcat extract-coords-for-single-art without-conjunctions)))

(defn get-year-from-act-name [s]
  (let [
        pattern (last (re-seq #"\s\d{4}\s" s))
        ]
        (when
          (not-empty pattern)
          (apply str
            (re-seq #"\d+" pattern)))))

(defn cut-to-first-parenthesis-pair [s]
  (re-find #"[^\(]*\([^\)]*\)" s))

(def journal-year-extraction-regex
  (re-pattern
   (str "Dz\\.\\s*U\\.\\s*z?\\s*\\d+\\s*r|"
        "Dz\\.\\s*U\\.\\s*\\d{4}|"
        "Dz\\.\\s*U\\.\\s*t\\.j\\.\\s*z\\s*\\d{4}")))

(defn get-year-of-law-act [s]
  (let [
        to-first-parenthesis-pair (cut-to-first-parenthesis-pair s)
        before-law-change-indication
          (if (nil? to-first-parenthesis-pair)
            s
            (first (str/split to-first-parenthesis-pair #"zm\." )))
        txt
          (if (nil? before-law-change-indication)
            s
            before-law-change-indication)
        year-pattern-match (re-find journal-year-extraction-regex txt)
        year
          (if
            (not-empty year-pattern-match)
            (apply str
                   (re-seq #"[\d]+" year-pattern-match))
            (get-year-from-act-name
             (first
              (str/split txt #"Dz\."))))
        ]
    year))

; Utilities for regexes matching

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

(def pl-big-diacritics "ĄĆĘŁŃÓŚŻŹ")
(def pl-diacritics (str "ąćęłńóśżź" pl-big-diacritics))
