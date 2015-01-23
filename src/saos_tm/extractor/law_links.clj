(ns saos-tm.extractor.law-links
  (:require
    [saos-tm.extractor.common :refer :all]
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    )
  (:import java.io.File)
  (:gen-class))

(defn in? [seq elm]
  (some #(= elm %) seq))

(def coords-tokens
  ["." "," "Art" "art" "ust" "par" "§" "pkt" "zd" "i"
  "oraz" "lub" "z" "-" "a" "także" "lit"])

(defn not-coords-nmb? [s]
  (let [
          result (re-matches #"((\d+)(-\d+)?[a-z]*)|([a-u])" s)
          ]
    (if (= result nil) true false))) 

(defn not-coord-token? [token]
  (let [
          result (in? coords-tokens token)
          ]
  (if (= result nil) true false)))

(defn get-coords-tokens [first-token-index tokens]
  (first
    (indices
      #(and (not-coord-token? %) (not-coords-nmb? %))
      (drop first-token-index tokens))))

(defn find-coords-ranges [first-token-index tokens]
  (let [
          first-non-coord-token-index
            (get-coords-tokens first-token-index tokens)
        ]
  [first-token-index
  (if (nil? first-non-coord-token-index)
    (inc (count tokens))
    (+ first-token-index first-non-coord-token-index))]))

(defn get-range [coll from to]
  (take (- to from) (drop from coll)))

(defn get-range* [coll fromto]
  (get-range coll (first fromto) (second fromto)))

(defn w-zwiazku-z? [tokens]
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

(defn handle-w-zwiazku-z [tokens-and-coords]
  (for [
          i (range 0 (count tokens-and-coords))
        ]
    (if (map? (nth tokens-and-coords i))  
      (nth tokens-and-coords i)
      (if (w-zwiazku-z? (nth tokens-and-coords i))
        (nth tokens-and-coords (+ i 1))
        (nth tokens-and-coords i)))))

(def dictionary-for-acts
  [
    [#"(?i)^Konstytucji" {:nr "78" :poz "483", :year "1997"}]
    [#"(?i)^k\.?c" {:nr "16" :poz "93", :year "1964"}]
    [#"(?i)^k\.?h" {:nr "57" :poz "502", :year "1934"}]
    [#"(?i)^k\.?k\.?s" {:nr "83" :poz "930", :year "1999"}]
    [#"(?i)^k\.?k\.?w" {:nr "90" :poz "557", :year "1997"}]
    [#"(?i)^k\.?k" {:nr "88" :poz "553", :year "1997"}]
    [#"(?i)^k\.?m" {:nr "138" :poz "1545", :year "2001"}]
    [#"(?i)^k\.?p\.?a" {:nr "30" :poz "168", :year "1960"}]
    [#"(?i)^k\.?p\.?c" {:nr "43" :poz "296", :year "1964"}]
    [#"(?i)^k\.?p\.?k" {:nr "89" :poz "555", :year "1997"}]
    [#"(?i)^k\.?p\.?w" {:nr "106" :poz "1148", :year ""}]
    [#"(?i)^k\.?p" {:nr "24" :poz "141", :year "1974"}]
    [#"(?i)^k\.?r\.?o" {:nr "9" :poz "59", :year "2001"}]
    [#"(?i)^k\.?s\.?h" {:nr "94" :poz "1037", :year "2000"}]
    [#"(?i)^k\.?w" {:nr "12" :poz "114", :year "1971"}]
    [#"(?i)^k\.?z" {:nr "82" :poz "598", :year "1933"}]
    [#"(?i)^u\.?s\.?p" {:nr "98" :poz "1070", :year "2001"}]
    [#"(?i)^ustawy o TK" {:nr "102" :poz "643", :year "1997"}]
    [#"(?i)^ustawy o Trybunale Konstytucyjnym" {:nr "102" :poz "643", :year "1997"}]
    [#"(?i)^ustawy o komornikach" {:nr "133" :poz "882", :year "1997"}]
    [#"(?i)^ustawy o ochronie konkurencji" {:nr "50" :poz "331", :year "2007"}]
    [#"(?i)^prawa o adwokat" {:nr "16" :poz "124", :year "1982"}]
    [#"(?i)^pzp" {:nr "19" :poz "177", :year "2004"}]
    [#"(?i)^ustawy pzp" {:nr "19" :poz "177", :year "2004"}]
    [#"(?i)^ustawy prawo zamówień publicznych" {:nr "19" :poz "177", :year "2004"}]
    [#"(?i)^Prawo zamówień publicznych" {:nr "19" :poz "177", :year "2004"}]
    [#"(?i)^prawa zamówień publicznych" {:nr "19" :poz "177", :year "2004"}]
    ])

(defn tokens-to-string [tokens]
  (let [ 
          txt (str/join " " tokens)
          without-unnecessary-spaces
            (replace-several txt
              #" \." "."
              #" ," ","
              #" / " "/"
              #"\( " "("
              #" \)" ")"
              #" ;" ";")
    ]
    without-unnecessary-spaces))

(def not-map? (complement map?))

(defn min-index [coll]
  (.indexOf coll
    (apply min coll)))

(defn regex-first-position [re s]
  (loop [m (re-matcher re s)]
   (if (.find m)
    (.start m))))

(defn extract-dictionary-case [tokens dictionary]
  (let [
          txt (tokens-to-string tokens)
          matched-indices
            (indices
              #(not-nil? (re-find % txt))
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
            (if (not-nil? first-index)
              (second
                (nth dictionary first-index))
              nil)
          ]
  (if (nil? dictionary-record)
    tokens
    dictionary-record)))

(defn act-without-poz? [nr-part last-nmb-nr-part-index]
  (and
    (>
      (count nr-part)
      (+ last-nmb-nr-part-index 2))
    (= "r"
      (nth nr-part (inc last-nmb-nr-part-index)))
    (= "."
      (nth nr-part
        (+ last-nmb-nr-part-index 2)))))

(defn extract-nr-poz-poz-present [parts]
  (let [
          nr-part (nth parts 0)
          poz-part (nth parts 2)
          poz
            (first
              (filter #(matches? % #"\d+") poz-part))
          last-nmb-nr-part-index
            (last
              (indices
                #(matches? % #"\d+")
                nr-part))
          nr
            (if
              (act-without-poz? nr-part last-nmb-nr-part-index)
              "0"
              (nth nr-part last-nmb-nr-part-index))
            ]
            (zipmap [:nr :poz] [nr poz])))

(defn extract-nr-poz [tokens]
  (let [
          parts (partition-by #(= "poz" %) tokens)
          ]
          (if (= 1 (count parts))
            (zipmap [:nr :poz] ["0" "0"])
            (extract-nr-poz-poz-present parts))))

(defn extract-year-nr-poz [tokens]
  (let [
            year (get-year-of-law-act (tokens-to-string tokens))
            nr-poz (extract-nr-poz tokens)
          ]
          (zipmap
            [:year :nr :poz]
            [year (:nr nr-poz) (:poz nr-poz)]))) 

(defn convert-year-to-full [year]
  (if (= (count year) 4)
    year
    (if
      (>
        (parse-int
          (str (first year)))
        1)
      (str "19" year)
      (str "20" year))))

(defn extract-nr-poz-dots [token]
  (let [
          numbers (drop 1 (str/split token #"\."))
          year (convert-year-to-full (first numbers))
          ]
          (zipmap
              [:year :nr :poz]
              [year (nth numbers 1) (nth numbers 2)])))

(defn extract-nr-poz-case [tokens dictionary]
  (if (some #{"Dz.U"} tokens)
    (let [
            indexOfDzU (.indexOf tokens "Dz.U")
            tokenAfterDzU (nth tokens (inc indexOfDzU))
          ]
          (if (matches? tokenAfterDzU #"(\.\d+)+")
            (extract-nr-poz-dots tokenAfterDzU)
            (extract-year-nr-poz tokens)))
    (if (some #{"Dz"} tokens)
      (extract-year-nr-poz tokens)
      (extract-dictionary-case tokens dictionary))))
    
(defn coord-to-text [token]
  (if (or (= "." token) (= "-" token))
    token
    (str " " token)))

(defn build-coords-text [tokens-range tokens]
  (str/replace
    (str/join ""
      (map
        coord-to-text
        (get-range* tokens tokens-range)))
    "- " "-"))

(defn get-interfering-art-coords-ranges [tokens]
  (map #(find-coords-ranges % tokens) 
    (indices 
      #(or (= % "art") (= % "Art") (= % "§")) 
      tokens)))

(defn get-inter-coords-ranges [tokens]
  (let [
          interfering-art-coords-ranges (get-interfering-art-coords-ranges tokens)
          ]
  (filter
    #(< (first %) (second %))
    (partition 2
      (concat
        (drop 1
          (flatten interfering-art-coords-ranges))
        [(count tokens)])))))

(defn get-correct-art-coords-ranges [tokens]
  (let [
          interfering-art-coords-ranges (get-interfering-art-coords-ranges tokens)
          inter-coords-ranges (get-inter-coords-ranges tokens)
          ]
  (partition 2
    (concat
    [(first (first interfering-art-coords-ranges))]
    (flatten inter-coords-ranges)))))

(defn extract-signature [s]
  (-> (str/replace s "Sygn." "")
    (str/replace "akt" "")
    (str/replace "(" "")
    (str/replace ")" "")
    (str/replace "*" "")
    (str/trim)))

(defn get-line-with-signature [s]
  (let [
          lines
            (str/split
              s
              (re-pattern system-newline))
          lines-with-sygn-text (filter #(.startsWith % "Sygn.") lines)
          index-of-first-line-ending-with-date
            (first
              (indices
                #(.endsWith % " r.")
                lines))
    ]
    (if (empty? lines-with-sygn-text)
      (nth lines (+ index-of-first-line-ending-with-date 1))
      (first lines-with-sygn-text))))

(def art-coords-names [:art :par :ust :pkt :zd :lit])

(defn create-map-for-art-coords [art-coords]
  (zipmap art-coords-names art-coords))

(defn get-data-for-act-art [art-act]
  (let [
          art (:art art-act)
          act (:act art-act)
    ]
    (map #(zipmap [:art :act] [(create-map-for-art-coords %1) act]) art)))

(defn get-data-for-orphaned-link [orphaned-link]
  (let [
        txt (tokens-to-string (:act orphaned-link))
        ]
    (map
      #(zipmap
        [:txt :art]
        [txt %])
      (:art orphaned-link))))

(defn load-dictionary [path]
  (let [
          txt (slurp path)
          lines (str/split txt (re-pattern system-newline))
          trimmed-lines
          (map
            #(subs % 1 (dec (count %)))
            lines)
          pattern
          (re-pattern
            (str "\"" csv-delimiter "\""))
          records
          (map
            #(str/split % pattern)
            trimmed-lines)
          dictionary
            (map
              #(vector
                (re-pattern
                  (replace-several (str "(?i)" (nth % 0))
                    #"\(" "\\("
                    #"\)" "\\)"))
                (zipmap
                  [:year :nr :poz]
                  [(nth % 1) (nth % 2) (nth % 3)]))
              records)
            ]
            dictionary))

(defn cleanse [s]
  (when (not-nil? s)
    (replace-several s
      #"\." ""
      #"\s" "")))

(defn cleanse-link [link]
  (let [
          art (:art link)
          act (:act link)
          ]
          (zipmap
            [:act :art]
            [(zipmap [:year :nr :poz]
                    [(cleanse (:year act))
                    (cleanse (:nr act))
                    (cleanse (:poz act))])
             (zipmap art-coords-names
                    [(cleanse (:art art))
                    (cleanse (:par art))
                    (cleanse (:ust art))
                    (cleanse (:pkt art))
                    (cleanse (:zd art))
                    (cleanse (:lit art))])])))
    
(defn extract-law-links [s dictionary]
  (let [
        merged-dictionary (concat dictionary dictionary-for-acts)
        txt (replace-several s
              #"art\." " art. "
              #"ust\." " ust. "
              #"§" " § "
              #"pkt" " pkt "
              #"zd\." " zd. ")
        tokens (split-to-tokens txt)
        interfering-art-coords-ranges
          (get-interfering-art-coords-ranges tokens)
        inter-coords-ranges
          (get-inter-coords-ranges tokens)
        correct-art-coords-ranges
          (get-correct-art-coords-ranges tokens)
        coords-texts
          (map
            #(build-coords-text % tokens)
            correct-art-coords-ranges)
        art-coords
          (map 
            extract-coords
            (map #(build-coords-text % tokens) correct-art-coords-ranges))
        act-coords
          (handle-w-zwiazku-z
            (map #(extract-nr-poz-case % merged-dictionary)
              (map 
                #(get-range tokens (first %) (second %)) 
                inter-coords-ranges)))
        links
          (distinct
            (map #(zipmap [:art :act] [%1 %2])
            art-coords act-coords))
        extracted-links
          (filter
            #(map? (:act %))
             links)
        extracted-links
          (mapcat get-data-for-act-art extracted-links)
        extracted-links
          (map #(cleanse-link %) extracted-links)
        orphaned-links
          (flatten
            (filter
              #(not-map? (:act %))
              links))
          ]
        (zipmap
          [:extracted-links :orphaned-links]
          [extracted-links
            (mapcat get-data-for-orphaned-link orphaned-links)])))
