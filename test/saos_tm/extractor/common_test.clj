(ns saos-tm.extractor.common-test
  (:require
    [clojure.test :refer :all]
    [ clojure.string :as str ]
    [ clojure-csv.core :refer :all ]
    [saos-tm.extractor.common :refer :all]))

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
      (get-precision-recall #{1 2} #{2 3 4 5}))))

(defn filter-ending-with [ss s]
  (sort
    (filter
      #(.endsWith (str %) s)
      ss)))

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

(defn read-files [ext ext-regex ext-files-paths]
  (let [
           ext-files (map #(slurp %) ext-files-paths)
           txt-files-paths
            (map
              #(str/replace % ext-regex ".txt")
              ext-files-paths)
           txt-files (map #(slurp %) txt-files-paths)
           ]
           (zipmap
             [:ext :txt]
             [ext-files txt-files])))

(defn get-precisions-recalls [coll1 coll2]
  (map
    #(get-precision-recall %1 %2)
    coll1
    coll2))

(def law-tests-data-path "test-data/")
(def log-data-path "log/")

(defn map-fn [func coll additional-item] 
  (map
    #(func % additional-item)
      coll))

(defn get-log-files-paths [ext-files-paths]
  (map
    #(str/replace
      (str %) law-tests-data-path log-data-path)
    ext-files-paths))

(defn spit-all-csv [result-to-csv-fn path data]
  (spit path
    (apply str
      (map-fn result-to-csv-fn data "signature"))))

(defn links-efficiency-test
  [ext ext-regex benchmark-records-fn extracted-records-fn
  precision-threshold recall-threshold result-to-csv-fn]
  (time
    (let [
             file-paths
               (.listFiles
                 (clojure.java.io/file law-tests-data-path))
             ext-files-paths (filter-ending-with file-paths ext)
             files (read-files ext ext-regex ext-files-paths)
             ext-files (:ext files)
             txt-files (:txt files)
             log-files-paths (get-log-files-paths ext-files-paths)
             _ (.mkdir (java.io.File. log-data-path))
             benchmark-items (benchmark-records-fn ext-files)
             extracted-items (extracted-records-fn txt-files)
             precisions-recalls
              (get-precisions-recalls extracted-items benchmark-items)
             precisions (get-elements :precision precisions-recalls)
             recalls (get-elements :recall precisions-recalls)
             average-precision (get-average precisions)
             average-recall (get-average recalls)
             ext-files-paths-str (map #(str %) ext-files-paths)
             names-precs-recalls
               (sort #(compare (last %1) (last %2)) 
                 (map
                   vector
                   ext-files-paths-str precisions recalls))
             _ (doseq [i names-precs-recalls] (println i))
             _ (println (str \newline "av. precision: " average-precision
                  " av. recall: " average-recall))
             _
              (doall
                (map
                  #(spit-all-csv result-to-csv-fn %1 %2)
                  log-files-paths
                  extracted-items))
       ]
       (is (> average-precision precision-threshold))
       (is (> average-recall recall-threshold)))))

