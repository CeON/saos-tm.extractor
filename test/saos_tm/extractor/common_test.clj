(ns saos-tm.extractor.common-test
  (:require
    [clojure.test :refer :all]
    [ clojure.string :as str ]
    [clojure.set :refer :all]
    [ clojure-csv.core :refer :all ]
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

(deftest article-coords-test
  (is(=
    [["4" "0" "0" "2" "0" "0"]]
    (extract-coords "art. 4 pkt 2")))
  (is(=
    [["14a" "0" "0" "0" "0" "0"]]
    (extract-coords "art. 14a ")))
  (is(=
    [["50" "0" "1" "1" "0" "0"]]
    (extract-coords "art. 50 ust. 1 pkt 1 ")))
  (is(=
    [["3" "0" "2-3" "0" "0" "0"]]
    (extract-coords "art. 3 ust. 2-3 ")))
  (is(=
    [["103-105" "0" "0" "0" "0" "0"]]
    (extract-coords "art. 103-105 ")))
  (is(=
    [["47" "0" "1" "2" "0" "0"] ["47" "0" "1" "3" "0" "0"]]
    (extract-coords "art. 47 ust. 1 pkt 2 i 3 ")))
  (is(=
    [["0" "44" "1" "1" "0" "0"]]
    (extract-coords "§ 44 ust. 1 pkt 1 ")))
  (is(=
    [["0" "25" "3" "0" "0" "0"]]
    (extract-coords "§ 25 ust. 3 ")))
  (is(=
    [["0" "68a" "1" "0" "0" "0"]]
    (extract-coords "§ 68a ust. 1 ")))
  (is(=
    [["0" "79" "0" "0" "0" "0"]]
    (extract-coords "§ 79 ")))
  (is(=
    [["0" "34" "3" "2" "0" "0"]]
    (extract-coords "§ 34 ust. 3 pkt 2 ")))
  (is(=
    [["37" "4a" "0" "0" "0" "0"] ["37" "4b" "0" "0" "0" "0"]]
    (extract-coords "art. 37 § 4a, 4b ")))
  (is(=
    [["56" "1-3" "0" "0" "0" "0"]]
    (extract-coords "art. 56 § 1-3 ")))
  (is(=
    [["77" "1" "0" "0" "0" "0"] ["77" "2" "0" "0" "0" "0"]
     ["77" "2a" "0" "0" "0" "0"] ["77" "3" "0" "0" "0" "0"]
     ["77" "3a" "0" "0" "0" "0"] ["77" "6" "0" "0" "0" "0"]
     ["77" "7a" "0" "0" "0" "0"] ["77" "7b" "0" "0" "0" "0"]]
    (extract-coords "art. 77 § 1, 2, 2a, 3, 3a, 6, 7a i 7b ")))
  (is(=
    [["46" "1" "0" "0" "1" "0"]]
    (extract-coords "art. 46 § 1 zd. 1 ")))
  (is(=
    [["178" "0" "1" "0" "0" "0"] ["91" "0" "1" "0" "0" "0"]]
    (extract-coords "art. 178 ust. 1 i art. 91 ust. 1")))
  (is(=
    [["84" "0" "0" "0" "0" "0"] ["92" "0" "1" "0" "0" "0"]
     ["31" "0" "3" "0" "0" "0"]]
    (extract-coords "art. 84, art. 92 ust. 1 i art. 31 ust. 3")))
  (is(=
    [["2" "0" "0" "0" "0" "0"] ["84" "0" "0" "0" "0" "0"]
     ["91" "0" "1" "0" "0" "0"] ["178" "0" "1" "0" "0" "0"]]
    (extract-coords "art. 2, art. 84, z art. 91 ust. 1, art. 178 ust. 1")))
  (is(=
    '(("64" "0" "2" "0" "0" "0") ("64" "0" "3" "0" "0" "0")
      ("84" "0" "0" "0" "0" "0"))
  	(extract-coords "art. 64 ust. 2 i 3 oraz art. 84")))
  (is(=
    '(("64" "0" "2" "0" "0" "a"))
    (extract-coords "art. 64 ust. 2 lit. a")))
  ; (is(= [[]] (extract-coords "")))
  )

(deftest precision-recall-measure-test
  (is
   (=
    {:precision 0.5 :recall 0.25}
    (get-precision-recall #{1 2} #{2 3 4 5}))
   (is
    (=
     {:precision 1.0 :recall 1.0}
     (get-precision-recall #{} #{}))
    )))

(defn split-coll [coll]
  (map
    #(str/trim %)
    coll))

(defn get-average [coll]
  (/ (reduce + coll) (count coll)))

(defn get-elements [key-name coll]
  (map
    #(key-name %)
    coll))

(defn firsts [coll]
  (map #(first %) coll))

(defn get-signature [file-data]
  (firsts
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
  (let [
        paths (list-file-paths dir)
        ]
    (map #(slurp %) paths)))

(defn get-precisions-recalls [coll1 coll2]
  (map
    #(get-precision-recall %1 %2)
    coll1
    coll2))

(def links-test-data-path "test-data/links/")
(def log-data-path "log/")

(defn map-fn [func coll additional-item]
  (map
    #(func % additional-item)
      coll))

(defn get-log-files-paths [ext-files-paths]
  (map
    #(str/replace
      (str %) links-test-data-path log-data-path)
    ext-files-paths))

(defn spit-all-csv [result-to-csv-fn path data]
  (spit path
    (apply str
      (sort
       (map-fn result-to-csv-fn data "signature")))))

(defn nils-to-zeros [coll]
  (map #(if (nil? %) 0 %) coll))

(defn links-efficiency-test
  [ext benchmark-records-fn extracted-records-fn txt-files-conv-fn
   precision-threshold recall-threshold result-to-csv-fn]
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
         precisions-recalls
           (get-precisions-recalls extracted-items benchmark-items)
         precisions
           (nils-to-zeros (get-elements :precision precisions-recalls))
         recalls
           (nils-to-zeros (get-elements :recall precisions-recalls))
         average-precision (get-average precisions)
         average-recall (get-average recalls)
         min-precision (apply min precisions)
         min-recall (apply min recalls)

         names-precs-recalls
           (sort
            #(compare (second %1) (second %2))
                 (map
                  vector
                  ext-files-names precisions recalls))
         _ (doseq [i names-precs-recalls] (println i))
         _ (println (str \newline "av. precision: " average-precision
                         " av. recall: " average-recall))
         _ (println (str "min precision: " min-precision
                         " min recall: " min-recall \newline))
         _
           (doall
            (map
             #(spit-all-csv result-to-csv-fn %1 %2)
             log-files-paths
             extracted-items))
         ]
     (is (> average-precision precision-threshold))
     (is (> average-recall recall-threshold)))))

(deftest dexmlise-test
  (is
    (=
      (dexmlise
        (str "<xBx> Szpitalowi <xAnon>(...)</xAnon> w <xAnon>Ł.</xAnon>"
          ", <xAnon>L. P. (1)</xAnon> i <xAnon>A. G. (1)</xAnon></xBx>"))
      " Szpitalowi (...) w Ł., L. P. (1) i A. G. (1)")))

(defn split-lines [s]
  (str/split s (re-pattern system-newline)))
