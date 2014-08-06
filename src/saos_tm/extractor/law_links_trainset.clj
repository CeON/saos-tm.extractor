(ns saos-tm.extractor.law-links-trainset
  (:require
    [ saos-tm.extractor.common :as common ]
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    )
  (:import java.io.File)
  (:gen-class))

(defn remove-new-lines [ s ]
  (str/replace s "\n" " "))

(defn get-law-decisions-index [ s ]
  (-> (str/split s #"I\s*(1\.)?\s*KONSTYTUCJA RZECZYPOSPOLITEJ POLSKIEJ")
    second
    (str/split #"(?i)PRAWO UNII? EUROPEJSKIEJ")
    first
    (str/split #"SKOROWIDZ RZECZOWY")
    first
    ))

(defn split-to-law-acts [ s ]
  (str/split
    s
    #"(UST(A|)WA)|(ROZPORZĄDZENIE)|(EUROPEJSKA)|(PRZEPISY)|(POSTANOWIENIE)"))

(defn get-numbers [ s ]
  (if (nil? s)
    nil
    (re-seq #"[\d]+" s)))

(defn get-nr-poz-strings [ s ]
  (re-find #"Nr\s*\d+,?.poz\.\s*\d+" s))

(defn get-nr-poz-of-law-act [ s ]
  (get-numbers
    (get-nr-poz-strings s)))

(defn get-description [ s ]
  (first
    (str/split s #"\(")))

(defn extract-signatures [ s ]
  (if (nil? s)
    nil
    (map #(str/replace % #"[ ]{2,}" " ")
      (map #(str/trim %)
        (map #(str/replace % #"sygn\." "")
          (map #(str/replace % #"poz\." "")
            (filter #((comp not common/substring?) "OTK" %)
            (filter #(common/substring? "/" %)
              (str/split s #",|\(poz")))))))))

(defn get-article-coordinates-info [ s ]
  (if (< (count (first (str/split s #"–"))) 40)
    (first (str/split s #"–"))
    ""))

(defn get-signatures-info [ s ]
  (second (str/split s #"–")))

(defn get-signatures-for-articles [ s ]
  [(concat (common/extract-coords (get-article-coordinates-info s)))
  (extract-signatures (get-signatures-info s))])

(defn get-article-nmbs-point-nmbs-signatures [ s ]
  (let [
          split-by-art
            (map
              #(str "art." %)
              (drop 1 (str/split s #"\sart\.")))]
  (map get-signatures-for-articles
    (if (empty? split-by-art)
      (map #(str "§" %) (drop 1 (str/split s #"\s§")))    
      split-by-art))))

(defn get-data-for-act [ s ]
  (let [
        nr-poz (get-nr-poz-of-law-act s) 
        ]
  (zipmap
    [:year :nr :pos :descript :art-coords]
    [(common/get-year-of-law-act s)
    (first nr-poz)
    (second nr-poz)
    (get-description s)
    (get-article-nmbs-point-nmbs-signatures s)])))

(defn get-links [articles-signatures]
  (for [ x (first articles-signatures)
    y (second articles-signatures)]
    [x y]))

(defn get-training-data [structure-record]
  (zipmap
    [:year :nr :pos :art-coords-signatures]
  [
   (structure-record :year)
   (structure-record :nr )
   (structure-record :pos)
   (mapcat get-links
    (concat (:art-coords structure-record)))]))

(defn join-acts-coords-with-article-coords [record]
  (let [
        act-coords [(:year record) (:nr record) (:pos record)]
        ]
    (map
      #(conj % act-coords)
      (:art-coords-signatures record))))

(defn to-csv [structure]
  (let [
          delim common/csv-delimiter
          art-coords (nth structure 0)
          art (nth art-coords 0)
          par (nth art-coords 1)
          ust (nth art-coords 2)
          pkt (nth art-coords 3)
          zd (nth art-coords 4)
          lit (nth art-coords 5)
          signature (nth structure 1)
          act-coords (nth structure 2)
          year (nth act-coords 0)
          nr (nth act-coords 1)
          poz (nth act-coords 2)
    ]
  ["\"" art "\"" delim "\"" par "\"" delim "\"" ust "\"" delim
   "\"" pkt "\"" delim "\"" zd "\"" delim "\"" lit "\"" delim
   "\"" signature "\"" delim
   "\"" year "\"" delim "\"" nr "\"" delim "\"" poz "\"" \newline]))

(defn handle-training-data [structure]
  (apply str
    (flatten
      (map to-csv
        (mapcat join-acts-coords-with-article-coords
          (map get-training-data structure))))))

(defn get-glossary-csv [structure]
  (apply str
    (map 
      #(str
        "\"" (:year %) "\""
        common/csv-delimiter
        "\"" (:nr %) "\""
        common/csv-delimiter
        "\"" (:pos %) "\""
        common/csv-delimiter
        "\"" (:descript %) "\""
        \newline)
      structure)))

(defn get-data-to-write [structure]
  [(get-glossary-csv structure) (handle-training-data structure)])

(defn write-data-to-files [training-set-path glossary-path structure]
  (spit glossary-path (first structure))
  (spit training-set-path (second structure)))

(defn process [input-file-path training-set-path glossary-path]
  (->> (slurp input-file-path)
    remove-new-lines
    get-law-decisions-index
    split-to-law-acts
    (map get-data-for-act)
    get-data-to-write
    (write-data-to-files training-set-path glossary-path)))
