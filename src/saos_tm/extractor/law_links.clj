(ns saos-tm.extractor.law-links
  (:require
   [saos-tm.extractor.common :as common]
   [clojure.string :as str]
   [clojure.set :as set]
   [langlab.core.parsers :refer [lg-split-tokens-bi]]
   [langlab.core.parsers :as langlab-parsers]
   [langlab.core.multi-stemmers :as langlab-multi-stemmers]
   [langlab.core.characters :as langlab-characters])
  (:import java.io.File)
  (:gen-class))

(def ^:private coords-tokens
  ["." "," ";" "Art" "art" "ust" "par" "§" "pkt" "zd" "i"
   "oraz" "lub" "z" "-" "a" "także" "lit"])

(def ^:private dictionary-for-acts
  [[#"(?i)^\s*Konstytucji"
    {:journalNo "78" :journalEntry "483", :journalYear "1997"}]
   [#"(?i)^\s*k\.?c"
    {:journalNo "16" :journalEntry "93", :journalYear "1964"}]
   [#"(?i)^\s*k\.?h"
    {:journalNo "57" :journalEntry "502", :journalYear "1934"}]
   [#"(?i)^\s*k\.?k\.?s"
    {:journalNo "83" :journalEntry "930", :journalYear "1999"}]
   [#"(?i)^\s*k\.?k\.?w"
    {:journalNo "90" :journalEntry "557", :journalYear "1997"}]
   [#"(?i)^\s*k\.?k"
    {:journalNo "88" :journalEntry "553", :journalYear "1997"}]
   [#"(?i)^\s*k\.?m"
    {:journalNo "138" :journalEntry "1545", :journalYear "2001"}]
   [#"(?i)^\s*k\.?p\.?a"
    {:journalNo "30" :journalEntry "168", :journalYear "1960"}]
   [#"(?i)^\s*k\.?p\.?c"
    {:journalNo "43" :journalEntry "296", :journalYear "1964"}]
   [#"(?i)^\s*k\.?p\.?k"
    {:journalNo "89" :journalEntry "555", :journalYear "1997"}]
   [#"(?i)^\s*k\.?p\.?w"
    {:journalNo "106" :journalEntry "1148", :journalYear "2001"}]
   [#"(?i)^\s*k\.?p"
    {:journalNo "24" :journalEntry "141", :journalYear "1974"}]
   [#"(?i)^\s*k\.?r\.?o"
    {:journalNo "9" :journalEntry "59", :journalYear "2001"}]
   [#"(?i)^\s*k\.?s\.?h"
    {:journalNo "94" :journalEntry "1037", :journalYear "2000"}]
   [#"(?i)^\s*k\.?w"
    {:journalNo "12" :journalEntry "114", :journalYear "1971"}]
   [#"(?i)^\s*k\.?z"
    {:journalNo "82" :journalEntry "598", :journalYear "1933"}]
   [#"(?i)^\s*u\.?s\.?p"
    {:journalNo "98" :journalEntry "1070", :journalYear "2001"}]
   [#"(?i)^\s*ustawy o TK"
    {:journalNo "102" :journalEntry "643", :journalYear "1997"}]
   [#"(?i)^\s*ustawy o Trybunale Konstytucyjnym"
    {:journalNo "102" :journalEntry "643", :journalYear "1997"}]
   [#"(?i)^\s*ustawy o komornikach"
    {:journalNo "133" :journalEntry "882", :journalYear "1997"}]
   [#"(?i)^\s*ustawy o ochronie konkurencji"
    {:journalNo "50" :journalEntry "331", :journalYear "2007"}]])

(def ^:private art-coords-names [:art :par :ust :pkt :zd :lit])

(def ^:private journal-regex #"[\S\s]*Dz\.\s*U\.[\S\s]*")

(def ^:private explicit-local-dictionary-definition-regex
  (re-pattern
   (str
    ";\\s*dalej\\s*:?|"
    ",\\s*dalej\\s*:?|"
    "zwana\\s*dalej\\s*:?|"
    "zwanej\\s*dalej\\s*:?|"
    "\\(dalej\\s*:?")))

(def ^:private acts-txts-split-regex
  (re-pattern
            (str "(?i)[^:]\\s+ustaw(a|y)\\s|"
                 "\\srozporządzen[^\\s]*\\s|"
                 "\\skodeksu\\s|"
                 "(A|a)rt\\.|"
                 "§")))

(def ^:private journal-year-extraction-regex
  (re-pattern
   (str "Dz\\.\\s*U\\.\\s*z?\\s*\\d+\\s*r|"
        "Dz\\.\\s*U\\.\\s*\\d{4}|"
        "Dz\\.\\s*U\\.\\s*t\\.j\\.\\s*z\\s*\\d{4}")))

(def ^:private cut-for-act-coords-regex
  (re-pattern
   (str "w\\s*zw\\.\\s*z|"
        "w\\s*związku\\s*z|"
        "[^e]\\s(U|u)staw|"
        "\\s(R|r)ozporządz|"
        "\\)")))

(def ^:private not-map? (complement map?))

(defn ^:private split-to-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn ^:private parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn ^:private in? [seq elm]
  (some #(= elm %) seq))

(defn ^:private not-coords-nmb? [s]
  (nil?
   (re-matches #"\d+(-\d+)?[a-z]*|[a-u]" s)))

(defn ^:private not-coord-token? [token]
  (nil?
   (in? coords-tokens token)))

(defn ^:private get-coords-tokens [first-token-index tokens]
  (first
   (common/indices
    #(and (not-coord-token? %) (not-coords-nmb? %))
    (drop first-token-index tokens))))

(defn ^:private find-coords-ranges [first-token-index tokens]
  (let [
        first-non-coord-token-index
          (get-coords-tokens first-token-index tokens)
        ]
    [first-token-index
     (if (nil? first-non-coord-token-index)
       (inc (count tokens))
       (+ first-token-index first-non-coord-token-index))]))

(defn ^:private get-range [coll from to]
  (take (- to from) (drop from coll)))

(defn ^:private get-range* [coll fromto]
  (get-range coll (first fromto) (second fromto)))

(defn ^:private w-zwiazku-z? [tokens]
  (or
   (and
    (= 3 (count tokens))
    (= "w" (nth tokens 0))
    (= "związku" (nth tokens 1))
    (= "z" (nth tokens 2)))
   (and
    (= 4 (count tokens))
    (= "w" (nth tokens 0))
    (= "zw" (nth tokens 1))
    (= "." (nth tokens 2))
    (= "z" (nth tokens 3)))))

(defn ^:private handle-w-zwiazku-z [tokens-and-coords]
  (for [
        i (range 0 (count tokens-and-coords))
        ]
    (if (map? (nth tokens-and-coords i))
      (nth tokens-and-coords i)
      (if
        (and
         (w-zwiazku-z? (nth tokens-and-coords i))
         (> (count tokens-and-coords) (inc i)))
        (nth tokens-and-coords (inc i))
        (nth tokens-and-coords i)))))

(defn ^:private tokens-to-string [tokens]
  (let [
        txt (str/join " " tokens)
        without-unnecessary-spaces
          (common/replace-several txt
                           #" \." "."
                           #" ," ","
                           #" / " "/"
                           #"\( " "("
                           #" \)" ")"
                           #" ;" ";")
        ]
    without-unnecessary-spaces))

(defn ^:private min-index [coll]
  (.indexOf coll
            (apply min coll)))

(defn ^:private regex-first-position [re s]
  (loop [m (re-matcher re s)]
    (if (.find m)
      (.start m))))

(defn ^:private are-coords? [item]
  (map? item))

(defn ^:private insert-at-index [s ch index]
  (str (apply str (take index s)) ch (apply str (drop index s))))

(defn ^:private handle-superscript-no-range [s]
  (let [
        length (count s)
        ]
    (if (and (> length 3) (langlab-characters/contains-digits-only? s))
      (str (insert-at-index s "(" 3) ")")
      s)))

(defn ^:private handle-superscript-range [s]
  (let [
        numbers
          (map handle-superscript-no-range
               (str/split s #"-"))
        ]
    (str/join "-" numbers)))

(defn ^:private handle-superscript [s]
  (if (common/substring? "-" s)
    (handle-superscript-range s)
    (handle-superscript-no-range s)))

(defn ^:private get-coords-names [is-art is-par is-ust is-pkt is-zd is-lit]
  (filter #(not= "" %)
    [(if is-art "art" "")
     (if is-par "par" "")
     (if is-ust "ust" "")
     (if is-pkt "pkt" "")
     (if is-zd "zd." "")
     (if is-lit "lit." "")]))

(defn ^:private zero-if-empty [item]
  (if (empty? item) "0" item))

(defn ^:private convert-ranges-to-single-records [record]
  (map
   #(zero-if-empty (record %))
   ["art" "par" "ust" "pkt" "zd." "lit."]))

(defn ^:private cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
      more (cartesian-product (rest colls))]
      (cons x more))))

(defn ^:private remove-trailing-conjunction [s]
  (str/replace s #"\si$|\sz$|\soraz$" ""))

(defn ^:private insert-art [s]
  (if (common/matches? s #"^(A|a)rt[\S\s]*")
    s
    (str "art. " s)))

(defn ^:private split-to-art-coords-parts [s]
  (str/split s #"Art\.?|art\.?|§|ust\.?|pkt\.?|zd\.?|lit\.?"))

(defn ^:private extract-coords [s]
  (let [
        numbers
          (map #(handle-superscript (str/trim %))
               (drop 1 (split-to-art-coords-parts s)))
        coords-names
          (get-coords-names
           (or (common/substring? "art" s) (common/substring? "Art" s))
           (common/substring? "§"   s)
           (common/substring? "ust" s)
           (common/substring? "pkt" s)
           (common/substring? "zd"  s)
           (common/substring? "lit" s))
        full-coords
           (convert-ranges-to-single-records
            (zipmap coords-names numbers))
        ]
    full-coords))

(defn ^:private create-coords-list-with-value-at-index
  [index coords-list-length s]
  (concat
   (take index (repeat "0"))
   [(str/trim s)]
   (take (dec (- coords-list-length index)) (repeat "0"))))

(defn ^:private cast-coords-lists [more-important-list less-important-list]
  (let [
        first-non-zero-index
          (count (take-while #(= % "0") less-important-list))
        ]
    (concat
     (take first-non-zero-index more-important-list)
     (drop first-non-zero-index less-important-list))))

(defn ^:private extract-coords-lists-for-other-parts
  [the-part first-part-coords]
  (if
    (or
     (common/substring? "§"   the-part)
     (common/substring? "ust" the-part)
     (common/substring? "pkt" the-part)
     (common/substring? "zd"  the-part)
     (common/substring? "lit" the-part))
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

(defn ^:private convert-to-coords-lists [parts-sep-by-conj]
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

(defn ^:private extract-coords-lists [s]
  (let [
        seperated-by-conjunctions (str/split s #",| i | oraz | lub ")
        coords (convert-to-coords-lists seperated-by-conjunctions)
        ]
    coords))

(defn ^:private extract-art-coords-with-multiple-art-numbers
  [art-part other-part]
  (let [
        art-parts (str/split art-part #",|\si\s|\soraz\s|\slub\s")
        last-part (str/join "" [(last art-parts) other-part])
        to-extract-coll (conj (drop-last art-parts) last-part)
        to-extract-with-art-coll (map insert-art to-extract-coll)
        ]
    (mapcat extract-coords-lists to-extract-with-art-coll)))

(defn ^:private extract-coords-for-single-art [s]
  (let [
        parts
          (langlab-parsers/split*
           (str/replace s #",$" "")
           #"§|ust\.|ust|pkt|zd\.|zd|lit\.|lit")
        art-part (first parts)
        other-part (str/join "" (rest parts))
        result
          (if (or
               (common/substring? "," art-part)
               (common/substring? " i " art-part)
               (common/substring? " oraz " art-part)
               (common/substring? " lub " art-part))
            (extract-art-coords-with-multiple-art-numbers art-part other-part)
            (extract-coords-lists (str/replace s #";$" "")))
        ]
    result))

(defn ^:private cleanse-commas [s]
  (common/remove-double-spaces
   (common/replace-several s
                    #"art\.?\s*," "art "
                    #"par\.?\s*," "par "
                    #"ust\.?\s*," "ust "
                    #"pkt\.?\s*," "pkt "
                    #"zd\.?\s*," "zd "
                    #"lit\.?\s*," "lit ")))

(defn ^:private extract-art-coords [s]
  (let [
        cleansed-commas (cleanse-commas s)
        trimmed (str/trim cleansed-commas)
        separate-art-coords
          (if (common/substring? "art" trimmed)
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

(defn ^:private get-year-from-act-name [s]
  (let [
        pattern (last (re-seq #"\s\d{4}\s" s))
        ]
        (when
          (not-empty pattern)
          (apply str
            (re-seq #"\d+" pattern)))))

(defn ^:private cut-to-first-parenthesis-pair [s]
  (re-find #"[^\(]*\([^\)]*\)" s))

(defn ^:private get-year-of-law-act [s]
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

(defn ^:private extract-with-global-dictionary [tokens-or-coords dictionary]
  (if (are-coords? tokens-or-coords)
    tokens-or-coords
    (let [
          txt (tokens-to-string tokens-or-coords)
          matched-indices
            (common/indices
             #(common/not-nil? (re-find % txt))
             (map #(first %) dictionary))
          positions
            (if-not (= 1 (count matched-indices))
              (map
               #(regex-first-position (first (nth dictionary %)) txt)
               matched-indices))
          min-i
            (if-not (= 1 (count matched-indices))
              (if-not (empty? positions)
                (min-index positions)))
          first-index
            (if-not (nil? min-i)
              (nth matched-indices min-i)
              (first matched-indices))
          dictionary-record
            (when (common/not-nil? first-index)
              (second
               (nth dictionary first-index)))
          ]
      (if (nil? dictionary-record)
        tokens-or-coords
        dictionary-record))))

(defn ^:private act-without-entry? [tokens index-of-last-nmb]
  (cond
   (nil? index-of-last-nmb)
   true
   :else
   (and
    (> (count tokens)
       (+ index-of-last-nmb 2))
    (= "r"
       (nth tokens (inc index-of-last-nmb)))
    (= "."
       (nth tokens
            (+ index-of-last-nmb 2))))))

(defn ^:private extract-when-entry-present [parts]
  (let [
        journal-nmb-part (nth parts 0)
        entry-part (nth parts 2)
        entry
          (first
           (filter #(common/matches? % #"\d+") entry-part))
        index-of-last-nmb
          (last
           (common/indices
            #(common/matches? % #"\d+")
            journal-nmb-part))
        journal-nmb
          (if
            (act-without-entry? journal-nmb-part index-of-last-nmb)
            "0"
            (nth journal-nmb-part index-of-last-nmb))
        ]
    (zipmap [:journalNo :journalEntry] [journal-nmb entry])))

(defn ^:private extract-journal-nmb-and-entry [tokens]
  (let [
        parts (partition-by #(= "poz" %) tokens)
        ]
    (if (= 1 (count parts))
      (zipmap [:journalNo :journalEntry] ["0" "0"])
      (extract-when-entry-present parts))))

(defn ^:private extract-year-journal-nmb-and-entry [tokens]
  (let [
        year (get-year-of-law-act (tokens-to-string tokens))
        journal-nmb-and-entry (extract-journal-nmb-and-entry tokens)
        ]
    (zipmap
     [:journalYear :journalNo :journalEntry]
     [year
      (:journalNo journal-nmb-and-entry)
      (:journalEntry journal-nmb-and-entry)])))

(defn ^:private convert-year-to-full [year]
  (if (= (count year) 4)
    year
    (if
      (>
       (parse-int
        (str (first year)))
       1)
      (str "19" year)
      (str "20" year))))

(defn ^:private extract-journal-nmb-and-entry-dots [token]
  (let [
        numbers (drop 1 (str/split token #"\."))
        year (convert-year-to-full (first numbers))
        ]
    (cond
     (= (count numbers) 3)
     (zipmap
      [:journalYear :journalNo :journalEntry]
      [year (nth numbers 1) (nth numbers 2)])
     (= (count numbers) 2)
     (zipmap
      [:journalYear :journalNo :journalEntry]
      [year 0 (nth numbers 1)])
     :else
     (zipmap
      [:journalYear :journalNo :journalEntry]
      [year 0 0]))))

(defn ^:private extract-act-coords-journal-with-dot [tokens]
  (let [
        index-of-journal-nmb-token (.indexOf tokens "Dz.U")
        token-after-journal-nmb (nth tokens (inc index-of-journal-nmb-token))
        ]
    (if (common/matches? token-after-journal-nmb #"(\.\d+)+")
      (extract-journal-nmb-and-entry-dots token-after-journal-nmb)
      (extract-year-journal-nmb-and-entry tokens))))

(defn ^:private stem [s]
  (let [
        morfologik-stems
          (when-not (nil? s)
            (langlab-multi-stemmers/pl-multi-stem-morfologik s))
        ]
    (if (empty? morfologik-stems)
      [s]
      morfologik-stems)))

(defn ^:private stems-match? [stems1 stems2]
  ((complement empty?) (set/intersection (set stems1) (set stems2))))

(defn ^:private tokens-match? [tokens1 tokens2]
  (let [
        stems1 (map stem tokens1)
        stems2 (map stem tokens2)
        consecutive-positions-matches
          (map #(stems-match? %1 %2) stems1 stems2)
        ]
    ((complement contains?) (set consecutive-positions-matches) false)))

(defn ^:private local-explicit-dictionary-item-matches? [item tokens]
  (let [
        dictionary-tokens-colls (:act-abbreviation item)
        matches (filter #(tokens-match? % tokens) dictionary-tokens-colls)
        ]
    ((complement empty?) matches)))

(defn ^:private check-second-and-third-token
  [first-match-index dictionary-stems dictionary-stems-count tokens]
  (cond
   (or
    (= (count tokens) 1)
    (= (count tokens) 2)
    (= (inc first-match-index) dictionary-stems-count))
   false
   :else
   (if
     (stems-match?
      (nth dictionary-stems (inc first-match-index))
      (stem (second tokens)))
     (or
      (= (+ 2 first-match-index) dictionary-stems-count)
      (stems-match?
       (nth dictionary-stems (+ 2 first-match-index))
       (stem (nth tokens 2))))
     false)))

(defn ^:private local-implicit-dictionary-item-matches? [item tokens]
  (let [
        dictionary-stems (:act-name item)
        first-match-index
          (first
           (common/indices
            #(stems-match? % (stem (first tokens)))
            dictionary-stems))
        dictionary-stems-count (count dictionary-stems)
        ]
    (if (nil? first-match-index)
      false
      (check-second-and-third-token
       first-match-index dictionary-stems dictionary-stems-count tokens))))

(defn ^:private extract-with-local-explicit-dictionary
  [tokens-or-coords dictionary]
  (if (are-coords? tokens-or-coords)
    tokens-or-coords
    (let [
          tokens-lowercase (map str/lower-case tokens-or-coords)
          matches
            (filter
             #(local-explicit-dictionary-item-matches? % tokens-lowercase)
             dictionary)
          ]
      (if (empty? matches)
        tokens-or-coords
        (:act-coords (first matches))))))

(defn ^:private extract-with-local-implicit-dictionary
  [tokens-or-coords dictionary]
  (if (are-coords? tokens-or-coords)
    tokens-or-coords
    (let [
          string (tokens-to-string tokens-or-coords)
          string-lowercase (str/lower-case string)
          string-lowercase-cut
            (str/replace string-lowercase #"^\s*ustaw[^\s]*\s+(o|z)?" "")
          matches
            (filter
             #(local-implicit-dictionary-item-matches?
               %
               (split-to-tokens string-lowercase-cut))
             dictionary)
          ]
      (if (empty? matches)
        tokens-or-coords
        (:act-coords (first matches))))))

(defn ^:private cut-tokens-for-act-coords [tokens]
  (let [
        string (tokens-to-string tokens)
        string-cut
          (first
           (str/split string cut-for-act-coords-regex))
        tokens-cut
          (when-not (empty? string-cut)
            (split-to-tokens string-cut))
        ]
    (if (empty? tokens-cut)
      tokens
      tokens-cut)))

(defn ^:private extract-with-dictionaries
  [local-explicit-dictionary local-implicit-dictionary global-dictionary
   tokens-cut]
  (let [
        extract-with-local-explicit
          (if ((complement empty?) local-explicit-dictionary)
            #(extract-with-local-explicit-dictionary
              % local-explicit-dictionary)
            identity)
        extract-with-local-implicit
          (if ((complement empty?) local-implicit-dictionary)
            #(extract-with-local-implicit-dictionary
              % local-implicit-dictionary)
            identity)
        extract-with-global
          (if ((complement empty?) global-dictionary)
            #(extract-with-global-dictionary
              % global-dictionary)
            identity)
        ]
    (-> tokens-cut
        extract-with-global
        extract-with-local-explicit
        extract-with-local-implicit)))

(defn ^:private extract-act-coords
  [tokens local-explicit-dictionary local-implicit-dictionary
   global-dictionary]
  (if
    (w-zwiazku-z? tokens)
    tokens
    (let [
          tokens-cut (cut-tokens-for-act-coords tokens)
          ]
      (cond
       (some #{"Dz.U"} tokens-cut)
       (extract-act-coords-journal-with-dot tokens)
       (and
        (some #{"Dz"} tokens-cut)
        (some #{"U"} tokens-cut))
       (extract-year-journal-nmb-and-entry tokens)
       :else
       (extract-with-dictionaries
        local-explicit-dictionary local-implicit-dictionary global-dictionary
        tokens-cut)))))

(defn ^:private coord-to-text [token]
  (if (or (= "." token) (= "-" token))
    token
    (str " " token)))

(defn ^:private build-coords-text [tokens-range tokens]
  (str/replace
   (str/join ""
             (map
              coord-to-text
              (get-range* tokens tokens-range)))
   "- " "-"))

(defn ^:private get-interfering-art-coords-ranges [tokens]
  (map
   #(find-coords-ranges % tokens)
   (common/indices
    #(or (= % "art") (= % "Art") (= % "§") (= % "artykuł") (= % "Artykuł"))
    tokens)))

(defn ^:private split-sum-of-colls-to-pairs [coll1 coll2]
    (partition 2 (concat coll1 coll2)))

(defn ^:private get-inter-coords-ranges-candidates
  [interfering-art-coords-ranges tokens]
  (split-sum-of-colls-to-pairs
   (drop 1 (flatten interfering-art-coords-ranges))
   [(count tokens)]))

(defn ^:private get-inter-coords-ranges [tokens]
  (let [
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        ]
    (filter
     #(< (first %) (second %))
     (get-inter-coords-ranges-candidates
      interfering-art-coords-ranges tokens))))

(defn ^:private get-correct-art-coords-ranges [tokens]
  (let [
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        inter-coords-ranges (get-inter-coords-ranges tokens)
        ]
    (split-sum-of-colls-to-pairs
     [(first (first interfering-art-coords-ranges))]
     (flatten inter-coords-ranges))))

(defn ^:private zip-link-to-map [link]
  (let [
        art (:art link)
        act (:act link)
        ]
    (map
     #(zipmap
       [:art :act]
       [(zipmap art-coords-names %1) act])
     art)))

(defn ^:private get-data-for-orphaned-link [orphaned-link]
  (let [
        txt (tokens-to-string (:act orphaned-link))
        ]
    (map
     #(zipmap
       [:txt :art]
       [txt %])
     (:art orphaned-link))))

(defn ^:private cleanse [s]
  (when (common/not-nil? s)
    (common/replace-several s
                     #"\." ""
                     #"\s" "")))

(defn ^:private cleanse-link [link]
  (let [
        art (:art link)
        act (:act link)
        ]
    (zipmap
     [:act :art]
     [(zipmap [:journalYear :journalNo :journalEntry]
              [(cleanse (:journalYear act))
               (cleanse (:journalNo act))
               (cleanse (:journalEntry act))])
      (zipmap art-coords-names
              [(cleanse (:art art))
               (cleanse (:par art))
               (cleanse (:ust art))
               (cleanse (:pkt art))
               (cleanse (:zd art))
               (cleanse (:lit art))])])))

(defn ^:private print-art-act-texts
  [tokens correct-art-coords-ranges inter-coords-ranges]
  (let [
        art-coords-texts
          (map
           #(build-coords-text % tokens)
           correct-art-coords-ranges)
        act-coords-texts
          (map
           #(build-coords-text % tokens)
           inter-coords-ranges)
        ]
    (doall
     (map
      #(prn (str %1 " | " %2))
      art-coords-texts
      act-coords-texts))))

(defn ^:private cleanse-act-abbrevation [s]
  (str/trim
   (common/replace-several s
                    #"^\s*również" ""
                    #"^[^A-Za-ząćęłńóśżźĄĆĘŁŃÓŚŻŹ\.]+" ""
                    #"[^A-Za-ząćęłńóśżźĄĆĘŁŃÓŚŻŹ\\.]+$" "")))

(defn ^:private extract-local-explicit-dictionary-item [parts]
  (let [
        act-abbreviation-txt (second parts)
        act-abbreviation-without-dash
          (str/replace act-abbreviation-txt #"^\s*–" "")
        act-abbreviation-cut
          (str/trim
           (first
            (str/split
             act-abbreviation-without-dash
             #":|\)|\.\s[A-ZĄĆĘŁŃÓŚŻŹ]|,|–|”\s[A-ZĄĆĘŁŃÓŚŻŹ]")))
        act-abbreviation-cleansed
          (cleanse-act-abbrevation act-abbreviation-cut)
        act-abbreviation-coll
          (if (common/substring? " lub " act-abbreviation-cleansed)
            (map cleanse-act-abbrevation
                 (str/split act-abbreviation-cleansed #"\slub\s"))
            [act-abbreviation-cleansed])
        act-abbreviation-coll-lowercase
          (map str/lower-case act-abbreviation-coll)
        act-abbreviation-coll-tokens
          (map split-to-tokens act-abbreviation-coll-lowercase)
        act-coords
          (extract-year-journal-nmb-and-entry
           (split-to-tokens (first parts)))
        ]
    {:act-coords act-coords
     :act-abbreviation act-abbreviation-coll-tokens}))

(defn ^:private doesnt-contain-journal? [s]
  (when (common/not-nil? s)
    ((complement common/matches?) s journal-regex)))

(defn ^:private cannot-extract-act-coords? [s]
  (or
   (empty? s)
   (doesnt-contain-journal?
    (first (str/split s #"w\s*zw\.\s*z|w\s*związku\s*z")))))

(defn ^:private get-local-explicit-dictionary-item [s]
  (when-not (cannot-extract-act-coords? s)
    (let [
          parts
            (str/split
             s
             explicit-local-dictionary-definition-regex)
          ]
      (when-not (= (count parts) 1)
        (extract-local-explicit-dictionary-item parts)))))

(defn ^:private split-to-journal-indication [s]
  (first
   (str/split s #"\(|\[|Dz.\s*U.")))

(defn ^:private extract-item [s]
  (let [
        lowercase (str/lower-case s)
        act-name
          (map stem
               (split-to-tokens
                (split-to-journal-indication lowercase)))
        act-coords
          (extract-year-journal-nmb-and-entry (split-to-tokens s))
        ]
    {:act-coords act-coords
     :act-name   act-name}))

(defn ^:private extract-local-implicit-dictionary-item [s]
  (let [
        to-first-parenthesis-pair (cut-to-first-parenthesis-pair s)
        ]
    (if (nil? to-first-parenthesis-pair)
      (extract-item s)
      (extract-item to-first-parenthesis-pair))))

(defn ^:private get-local-implicit-dictionary-item [s]
  (when-not (cannot-extract-act-coords? s)
    (extract-local-implicit-dictionary-item s)))

(defn ^:private get-local-dictionary [acts-txts get-dictionary-item-fn]
  (let [
        local-dictionary-with-nils
          (map get-dictionary-item-fn acts-txts)
        local-dictionary
          (remove nil? local-dictionary-with-nils)
        ]
    local-dictionary))

(defn ^:private return-empty-coll [arg1 arg2] [])

(defn ^:private get-range-from-first-law-act-to-first-article-token
  [tokens first-art-index]
  (when-not (nil? first-art-index)
    (let [
          first-law-act-token-index
            (first
             (common/indices
             #(or (= "ustawa" %) (= "ustawy" %))
             (get-range tokens 0 first-art-index)))
          ]
      (when-not (nil? first-law-act-token-index)
        [first-law-act-token-index first-art-index]))))

(defn extract-law-links
  [s
   use-local-explicit-dictionary use-local-implicit-dictionary
   use-global-dictionary]
  (let [
        extract-local-explicit-dicitionary-fn
          (if use-local-explicit-dictionary
            #(get-local-dictionary % get-local-explicit-dictionary-item)
            #(return-empty-coll % nil))
        extract-local-implicit-dicitionary-fn
          (if use-local-implicit-dictionary
            #(get-local-dictionary % get-local-implicit-dictionary-item)
            #(return-empty-coll % nil))
        global-dictionary
          (if use-global-dictionary
            dictionary-for-acts
            [])

        preprocessed (common/preprocess s)
        txt (common/replace-several preprocessed
                             #"art\." " art. "
                             #"ust\." " ust. "
                             #"§" " § "
                             #"pkt" " pkt "
                             #"zd\." " zd. "
                             #"poz\." " poz. "
                             #"art\.?\s*art\.?" " art. ")
        tokens (split-to-tokens txt)
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        inter-coords-ranges
          (get-inter-coords-ranges tokens)
        correct-art-coords-ranges
          (get-correct-art-coords-ranges tokens)
        art-coords
          (map
           extract-art-coords
           (map #(build-coords-text % tokens) correct-art-coords-ranges))
        range-from-first-law-act-to-first-article-token
          (get-range-from-first-law-act-to-first-article-token
           tokens
           (first (first inter-coords-ranges)))
        acts-txts (drop 1 (str/split txt acts-txts-split-regex))
        act-coords
          (handle-w-zwiazku-z
           (map
            #(extract-act-coords
              %
              (extract-local-explicit-dicitionary-fn acts-txts)
              (extract-local-implicit-dicitionary-fn acts-txts)
              global-dictionary)
            (map
             #(get-range tokens (first %) (second %))
             inter-coords-ranges)))
        links
          (distinct
           (map #(zipmap [:art :act] [%1 %2])
                art-coords act-coords))
        extracted-links (filter #(map? (:act %)) links)
        extracted-links-maps (mapcat zip-link-to-map extracted-links)
        extracted-links-cleansed (map cleanse-link extracted-links-maps)
        orphaned-links
          (flatten
           (filter
            #(not-map? (:act %))
            links))
        ]
    (zipmap
     [:extracted-links :orphaned-links]
     [ (into [] extracted-links-cleansed)
       (into []
             (mapcat get-data-for-orphaned-link orphaned-links))])))

; Utilities for the art part of the extracted link.
; Includes converting to string and sorting.

(defn ^:private append-prefix-and-suffix-if-suffix-not-zero
  [ ^String s ^String prefix  ^String suffix ]
  (if-not (= suffix "0")
    (str s " " prefix " " suffix)
    s))

(defn ^:private convert-art-to-str [ art ]
  (-> ""
    (append-prefix-and-suffix-if-suffix-not-zero "art." (:art art))
    (append-prefix-and-suffix-if-suffix-not-zero "§" (:par art))
    (append-prefix-and-suffix-if-suffix-not-zero "ust." (:ust art))
    (append-prefix-and-suffix-if-suffix-not-zero "pkt" (:pkt art))
    (append-prefix-and-suffix-if-suffix-not-zero "zd." (:zd art))
    (append-prefix-and-suffix-if-suffix-not-zero "lit." (:lit art))
    (str/trim)))

(defn ^:private chain-compare [ res s1 s2 ]
  (if (= 0 res)
    (compare s1 s2)
    res))

(defn ^:private append-zero-if-no-letter [s]
  (if-not (re-find #"[a-z]$" s)
    (str s "0")
    s))

(defn ^:private add-spaces ^:private [n s]
  (str (apply str (repeat n " ")) s))

(defn ^:private conv-number-letter-for-comparison [ s1 s2 ]
  (let [
        s1* (append-zero-if-no-letter s1)
        s2* (append-zero-if-no-letter s2)
        n (- (.length s1*) (.length s2*))
        ]
    [ (add-spaces (- n) s1*) (add-spaces n s2*)]))

(defn ^:private chain-compare-number-letter [ res s1 s2 ]
  (if (= 0 res)
    (apply compare
      (conv-number-letter-for-comparison s1 s2))
    res))

(defn ^:private compare-arts [ art1 art2 ]
  (-> 0
    (chain-compare-number-letter (:art art1) (:art art2))
    (chain-compare-number-letter (:par art1) (:par art2))
    (chain-compare-number-letter (:ust art1) (:ust art2))
    (chain-compare-number-letter (:pkt art1) (:pkt art2))
    (chain-compare (:lit art1) (:lit art2))
    (chain-compare (:zd art1) (:zd art2))))

(defn ^:private sort-arts [ arts ]
  (sort-by identity compare-arts arts))

; Utilities for the act part of the extracted link.

(defn ^:private conv-act-to-str [ act ]
  (str
    "Dz. U. z "
    (:journalYear act) " r."
    (when-let [ no (:journalNo act)]
      (str " Nr " no))
    " poz. " (:journalEntry act)))
