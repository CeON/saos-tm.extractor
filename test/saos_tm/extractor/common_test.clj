(ns saos-tm.extractor.common-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [clojure-csv.core :refer :all]
   [saos-tm.extractor.law-links :refer :all]
   [saos-tm.extractor.common :refer :all]))

(defn get-file-paths [dir re]
  (let [
          file-paths
            (.listFiles
              (clojure.java.io/file dir))
          file-paths
            (filter #(matches? (str %) re)
                    file-paths)
        ]
    file-paths))

(defn get-file-names [dir re]
  (let [
        sorted-paths (sort (get-file-paths dir re))
        ]
    (map #(last (str/split (str %) #"/")) sorted-paths)))

(defn get-file-contents [dir re]
  (let [
        sorted-paths (sort (get-file-paths dir re))
        ]
    (map #(slurp %) sorted-paths)))

(defn get-precision-recall [extracted-set benchmark-set]
  (if (and (empty? extracted-set) (empty? benchmark-set))
    (zipmap [:precision :recall] [1.0 1.0])
    (let [
          true-positives-count
            (count
             (intersection extracted-set benchmark-set))
          extracted-count (count extracted-set)
          benchmark-count (count benchmark-set)
          precision (get-measure true-positives-count extracted-count)
          recall (get-measure true-positives-count benchmark-count)
          ]
      (zipmap [:precision :recall] [precision recall]))))

(defn read-law-links-to-maps [file-data]
  (let [
        data (parse-csv file-data)
        ]
    (into #{} (map
               #(zipmap
                 [:art :act]
                 [(zipmap
                   [:art :par :ust :pkt :zd :lit]
                   (take 6 %))
                  (zipmap
                   [:journalYear :journalNo :journalEntry]
                   (take-last 3 %))])
               data))))

(defn get-benchmark-records [files]
  (map
    read-law-links-to-maps
    files))

(defn law-links-extract [txt-files extract-law-links-fn]
  (let [
        dictionary (load-dictionary (io/resource "act_dictionary.txt"))
        ]
    (map
      #(into #{} (:extracted-links (extract-law-links-fn % dictionary)))
      txt-files)))

(defn law-links-extract-greedy [txt-files]
  (law-links-extract txt-files extract-law-links-greedy))

(defn law-links-extract-strict [txt-files]
  (law-links-extract txt-files extract-law-links-strict))

(deftest article-coords-test
  (is(=
      [["4" "0" "0" "2" "0" "0"]]
      (extract-art-coords "art. 4 pkt 2")))
  (is(=
      [["14a" "0" "0" "0" "0" "0"]]
      (extract-art-coords "art. 14a ")))
  (is(=
      [["50" "0" "1" "1" "0" "0"]]
      (extract-art-coords "art. 50 ust. 1 pkt 1 ")))
  (is(=
      [["3" "0" "2-3" "0" "0" "0"]]
      (extract-art-coords "art. 3 ust. 2-3 ")))
  (is(=
      [["103-105" "0" "0" "0" "0" "0"]]
      (extract-art-coords "art. 103-105 ")))
  (is(=
      [["47" "0" "1" "2" "0" "0"] ["47" "0" "1" "3" "0" "0"]]
      (extract-art-coords "art. 47 ust. 1 pkt 2 i 3 ")))
  (is(=
      [["0" "44" "1" "1" "0" "0"]]
      (extract-art-coords "§ 44 ust. 1 pkt 1 ")))
  (is(=
      [["0" "25" "3" "0" "0" "0"]]
      (extract-art-coords "§ 25 ust. 3 ")))
  (is(=
      [["0" "68a" "1" "0" "0" "0"]]
      (extract-art-coords "§ 68a ust. 1 ")))
  (is(=
      [["0" "79" "0" "0" "0" "0"]]
      (extract-art-coords "§ 79 ")))
  (is(=
      [["0" "34" "3" "2" "0" "0"]]
      (extract-art-coords "§ 34 ust. 3 pkt 2 ")))
  (is(=
      [["37" "4a" "0" "0" "0" "0"] ["37" "4b" "0" "0" "0" "0"]]
      (extract-art-coords "art. 37 § 4a, 4b ")))
  (is(=
      [["56" "1-3" "0" "0" "0" "0"]]
      (extract-art-coords "art. 56 § 1-3 ")))
  (is(=
      [["77" "1" "0" "0" "0" "0"] ["77" "2" "0" "0" "0" "0"]
       ["77" "2a" "0" "0" "0" "0"] ["77" "3" "0" "0" "0" "0"]
       ["77" "3a" "0" "0" "0" "0"] ["77" "6" "0" "0" "0" "0"]
       ["77" "7a" "0" "0" "0" "0"] ["77" "7b" "0" "0" "0" "0"]]
      (extract-art-coords "art. 77 § 1, 2, 2a, 3, 3a, 6, 7a i 7b ")))
  (is(=
      [["46" "1" "0" "0" "1" "0"]]
      (extract-art-coords "art. 46 § 1 zd. 1 ")))
  (is(=
      [["178" "0" "1" "0" "0" "0"] ["91" "0" "1" "0" "0" "0"]]
      (extract-art-coords "art. 178 ust. 1 i art. 91 ust. 1")))
  (is(=
      [["84" "0" "0" "0" "0" "0"] ["92" "0" "1" "0" "0" "0"]
       ["31" "0" "3" "0" "0" "0"]]
      (extract-art-coords "art. 84, art. 92 ust. 1 i art. 31 ust. 3")))
  (is(=
      [["2" "0" "0" "0" "0" "0"] ["84" "0" "0" "0" "0" "0"]
       ["91" "0" "1" "0" "0" "0"] ["178" "0" "1" "0" "0" "0"]]
      (extract-art-coords
       "art. 2, art. 84, z art. 91 ust. 1, art. 178 ust. 1")))
  (is(=
      '(("64" "0" "2" "0" "0" "0") ("64" "0" "3" "0" "0" "0")
        ("84" "0" "0" "0" "0" "0"))
      (extract-art-coords "art. 64 ust. 2 i 3 oraz art. 84")))
  (is(=
      '(("64" "0" "2" "0" "0" "a"))
      (extract-art-coords "art. 64 ust. 2 lit. a")))
  ; (is(= [[]] (extract-art-coords "")))
  )

(defn get-precision-recall [extracted-set benchmark-set]
  (if (and (empty? extracted-set) (empty? benchmark-set))
    {:precision 1.0 :recall 1.0}
    (let [
          true-positives-count
            (count
             (intersection extracted-set benchmark-set))
          extracted-count (count extracted-set)
          benchmark-count (count benchmark-set)
          precision (get-measure true-positives-count extracted-count)
          recall (get-measure true-positives-count benchmark-count)
          ]
      {:precision precision :recall recall})))

(deftest precision-recall-measure-test
  (is
   (=
    {:precision 0.5 :recall 0.25}
    (get-precision-recall #{1 2} #{2 3 4 5})))
   (is
    (=
     {:precision 1.0 :recall 1.0}
     (get-precision-recall #{} #{}))))

(deftest unsplit-words-across-lines-test
  (is (= (unsplit-words-across-lines "postę-\npowania") "postępowania")))

(deftest preprocess-test
  (is (= (preprocess "postę-\npowania") "postępowania")))

(defn get-file-paths [dir re]
  (let [
        file-paths
          (.listFiles
           (clojure.java.io/file dir))
        file-paths
          (filter #(matches? (str %) re)
                  file-paths)
        ]
    file-paths))

(defn get-file-names [dir re]
  (let [
        sorted-paths (sort (get-file-paths dir re))
        ]
    (map #(last (str/split (str %) #"/")) sorted-paths)))

(defn get-file-contents [dir re]
  (let [
        sorted-paths (sort (get-file-paths dir re))
        ]
    (map slurp sorted-paths)))

(defn get-average [coll]
  (/ (reduce + coll) (count coll)))

(defn get-elements [key-name coll]
  (map
   #(key-name %)
   coll))

(defn get-signature [file-data]
  (map first
       (parse-csv file-data)))

(defn list-file-paths [dir]
  (sort
    (.listFiles
     (clojure.java.io/file dir))))

(defn list-file-names [dir]
  (sort
    (.list
     (clojure.java.io/file dir))))

(defn get-files-from-dir [dir]
  (map
   #(slurp %)
   (sort (list-file-paths dir))))

(defn map-fn [func coll additional-item]
  (map
   #(func % additional-item)
   coll))

(defn spit-all-csv [result-to-csv-fn path data]
  (spit path
        (apply str
               (sort
                (map-fn result-to-csv-fn data "signature")))))

(defn nils-to-zeros [coll]
  (map #(if (nil? %) 0 %) coll))

(defn zip-with-file-name [file-name coll]
  (map
   #(zipmap
     [:fileName :link]
     [file-name %])
   coll))

(defn get-items-with-file-names [file-names items]
  (set
   (mapcat zip-with-file-name file-names items)))

(defn split-lines [s]
  (str/split s (re-pattern system-newline)))

(def links-test-data-path "test-data/links/")
(def log-data-path "log/")

(defn spit-all-csv-with-signatures [result-to-csv-fn path data signature]
  (spit path
        (apply str
               (sort
                (map-fn result-to-csv-fn data signature)))))

(defn split-csv-line [s]
  (str/split s (re-pattern (str #"\"" csv-delimiter "\""))))

(defn extract-signatures-from-csv [txts]
  (map
   #(nth (split-csv-line %) 6)
   txts))

(defn log-results-with-signatures
  [result-to-csv-fn log-files-paths extracted-items ext-files]
  (doall
   (map
    #(spit-all-csv-with-signatures result-to-csv-fn %1 %2 %3)
    log-files-paths
    extracted-items
    (extract-signatures-from-csv ext-files))))

(defn log-results-without-signatures
  [result-to-csv-fn log-files-paths extracted-items ext-files]
  (doall
   (map
    #(spit-all-csv result-to-csv-fn %1 %2)
    log-files-paths
    extracted-items)))

(defn remove-page-nmbs [s]
  (str/replace s
               (re-pattern
                (str system-newline "\\d+" system-newline))
               "\n"))

(defn links-preprocess [coll]
  (map remove-page-nmbs coll))

(defn get-and-print-efficiencies
  [benchmark-items extracted-items
   ext-files ext-files-names log-files-paths
   log-results-fn result-to-csv-fn
   description]
  (let [
        file-names (map #(first (str/split % #"\.")) ext-files-names)
        benchmark-items-with-file-names
          (get-items-with-file-names file-names benchmark-items)
        extracted-items-with-file-names
          (get-items-with-file-names file-names extracted-items)

        precisions-recalls
          (map get-precision-recall extracted-items benchmark-items)
        precisions
          (nils-to-zeros (get-elements :precision precisions-recalls))
        recalls
          (nils-to-zeros (get-elements :recall precisions-recalls))
        average-precision (get-average precisions)
        average-recall (get-average recalls)
        min-precision (apply min precisions)
        min-recall (apply min recalls)
        overall-precision-recall
          (get-precision-recall
           extracted-items-with-file-names benchmark-items-with-file-names)
        counts (map #(* (count %1) (- 1.0 %2)) extracted-items precisions)

        names-precs-recalls
          (sort
           #(compare (nth %1 3) (nth %2 3))
           (map
            vector
            ext-files-names precisions recalls counts))

        _ (prn)
        _ (prn description)
        _ (prn)

        _ (doseq [i names-precs-recalls] (println i))
        _ (println (str \newline "av. precision: " average-precision
                        " av. recall: " average-recall))
        _ (println (str "min precision: " min-precision
                        " min recall: " min-recall \newline))

        separator (str/join "" (take 70 (repeat "-")))
        _ (println (str separator \newline))
        _ (println (str "OVERALL PRECISION: "
                        (:precision overall-precision-recall)
                        " RECALL: "
                        (:recall overall-precision-recall)
                        \newline))
        _ (println (str separator \newline))

        _
          (log-results-fn
           result-to-csv-fn log-files-paths extracted-items ext-files)
        ]
    overall-precision-recall))

(defn links-efficiency-test
  [ext benchmark-records-fn extracted-records-fn txt-files-conv-fn
   precision-threshold recall-threshold result-to-csv-fn log-results-fn]
  (time
   (let [
         ext-dir (str links-test-data-path ext)
         ext-files (get-files-from-dir ext-dir)
         txt-files
           (txt-files-conv-fn
            (get-files-from-dir
             (str links-test-data-path "txt/")))
         ext-files-names (list-file-names ext-dir)
         log-files-paths
           (sort
             (map
              #(str log-data-path ext "/" %)
              ext-files-names))
         _ (.mkdir (java.io.File. (str log-data-path ext)))

         benchmark-items (benchmark-records-fn ext-files)
         extracted-items (extracted-records-fn txt-files)

         overall-precision-recall
           (get-and-print-efficiencies
            benchmark-items extracted-items
            ext-files ext-files-names log-files-paths
            log-results-fn result-to-csv-fn
            "OVERALL")
         ]
     (is (> (:precision overall-precision-recall) precision-threshold))
     (is (> (:recall overall-precision-recall) recall-threshold)))))

(defn extract-elems [key-name coll]
  (set
   (map #(key-name %) coll)))

(defn law-links-efficiency-test
  [ext benchmark-records-fn extracted-records-fn txt-files-conv-fn
   acts-precision-threshold acts-recall-threshold
   arts-precision-threshold arts-recall-threshold
   overall-precision-threshold overall-recall-threshold
   result-to-csv-fn log-results-fn]
  (time
   (let [
         ext-dir (str links-test-data-path ext)
         ext-files (get-files-from-dir ext-dir)
         txt-files
           (txt-files-conv-fn
            (get-files-from-dir
             (str links-test-data-path "txt/")))
         ext-files-names (list-file-names ext-dir)
         log-files-paths
           (sort
             (map
              #(str log-data-path ext "/" %)
              ext-files-names))
         _ (.mkdir (java.io.File. (str log-data-path ext)))

         benchmark-items (benchmark-records-fn ext-files)
         benchmark-acts (map #(extract-elems :act %) benchmark-items)
         benchmark-arts (map #(extract-elems :art %) benchmark-items)

         extracted-items (extracted-records-fn txt-files)
         extracted-acts (map #(extract-elems :act %) extracted-items)
         extracted-arts (map #(extract-elems :art %) extracted-items)

         acts-precision-recall
           (get-and-print-efficiencies
            benchmark-acts extracted-acts
            ext-files ext-files-names log-files-paths
            log-results-fn result-to-csv-fn
            "ACTS")

         arts-precision-recall
           (get-and-print-efficiencies
            benchmark-arts extracted-arts
            ext-files ext-files-names log-files-paths
            log-results-fn result-to-csv-fn
            "ARTS")

         overall-precision-recall
           (get-and-print-efficiencies
            benchmark-items extracted-items
            ext-files ext-files-names log-files-paths
            log-results-fn result-to-csv-fn
            "ACTS+ARTS")
         ]
     (is (> (:precision overall-precision-recall) overall-precision-threshold))
     (is (> (:recall overall-precision-recall) overall-recall-threshold))
     (is (> (:precision acts-precision-recall) acts-precision-threshold))
     (is (> (:recall acts-precision-recall) acts-recall-threshold))
     (is (> (:precision arts-precision-recall) arts-precision-threshold))
     (is (> (:recall arts-precision-recall) arts-recall-threshold))
     )))

(deftest dexmlise-test
  (is
   (=
    (dexmlise
     (str "<xBx> Szpitalowi <xAnon>(...)</xAnon> w <xAnon>Ł.</xAnon>"
          ", <xAnon>L. P. (1)</xAnon> i <xAnon>A. G. (1)</xAnon></xBx>"))
    " Szpitalowi (...) w Ł., L. P. (1) i A. G. (1)")))
