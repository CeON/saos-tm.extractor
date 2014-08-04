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

(defn get-year-from-act-name [s]
  (let [
        pattern (re-find #"\d+\s+r" s)
        ]
  (when
    (not-empty pattern)
    (apply str
      (re-seq #"[\d]+" pattern)))))

(defn get-year-of-law-act [s]
  (let [
          pattern (re-find #"Dz\.\s+U\.\s+z?\s+\d+\s+r" s)
          ]
  (if
    (not-empty pattern)
    (apply str
      (re-seq #"[\d]+" pattern))
    (get-year-from-act-name s))))

(defn get-data-for-act [ s ]
  (println s)
  (println "----------------------------------")
  (println (get-year-of-law-act s))
  (println "==================================")
  (zipmap
    [:year :nr-poz :descript :art-act-sign]
    [(get-year-of-law-act s)
    (get-nr-poz-of-law-act s)
    (get-description s)
    (get-article-nmbs-point-nmbs-signatures s)]))

(defn get-links [articles-signatures]
  (for [ x (first articles-signatures)
    y (second articles-signatures)]
    [x y]))

(defn get-training-data [structure-record]
  [(first structure-record)
   (mapcat get-links
    (concat (:art-act-sign structure-record)))])

(defn join-acts-coords-with-article-coords [record]
  (let [
        act-coords (first record)
        ]
    (map
      #(conj % act-coords)
      (second record))))

(defn to-csv [structure]
  [(apply str
    (map
      #(str "\"" % "\"" common/csv-delimiter)
      (first structure)))
   (apply str
    (map
      #(str "\"" % "\"" common/csv-delimiter)
      (nth structure 2)))
   (apply str
    "\"" (second structure) "\"")
   \newline])

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
        "\"" (first (:nr-poz %)) "\""
        common/csv-delimiter
        "\"" (second (:nr-poz %)) "\""
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
