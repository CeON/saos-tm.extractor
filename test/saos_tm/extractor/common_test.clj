(ns saos-tm.extractor.common-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [clojure-csv.core :refer :all]
   [saos-tm.extractor.law-links :refer :all]
   [saos-tm.extractor.judgment-links :refer :all]
   [saos-tm.extractor.common :refer :all]))

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

(defn get-measure [true-positives-count elements-count]
  (if
    (= elements-count 0)
    nil
    (float (/ true-positives-count elements-count))))

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
  (map
   #(into #{} (:extracted-links (extract-law-links-fn %)))
   txt-files))

(defn judgment-links-extract [txt-files]
  (map
    #(extract-all-signatures %)
    txt-files))

(defn signature-to-csv [signature not-used]
  (apply str "\"" signature "\"" system-newline))

(defn law-links-extract-greedy [txt-files]
  (law-links-extract txt-files #(extract-law-links-greedy % true true true)))

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
      (extract-art-coords "art. 64 ust. 2 lit. a"))))

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

(defn get-benchmark-signatures [ext-files]
  (let [
        jdg-signatures
          (map
           #(get-signature %)
           ext-files)
        benchmark-signatures
          (map
           #(set (remove empty? (map str/trim %)))
           jdg-signatures)
        ]
    benchmark-signatures))

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
        counts-precs
          (map #(* (count %1) (- 1.0 %2)) extracted-items precisions)
        counts-recalls
          (map #(* (count %1) (- 1.0 %2)) benchmark-items recalls)

        names-precs-recalls
          (sort
           #(compare (nth %1 4) (nth %2 4))
           (map
            vector
            ext-files-names precisions recalls counts-precs counts-recalls))

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
  [txt-dir-name ext
   benchmark-records-fn extracted-records-fn txt-files-conv-fn
   precision-threshold recall-threshold result-to-csv-fn log-results-fn]
  (time
   (let [
         ext-dir (str links-test-data-path ext)
         ext-files (get-files-from-dir ext-dir)
         txt-files
           (txt-files-conv-fn
            (get-files-from-dir
             (str links-test-data-path txt-dir-name "/")))
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
  [txt-dir-name ext
   benchmark-records-fn extracted-records-fn txt-files-conv-fn
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
             (str links-test-data-path txt-dir-name "/")))
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
     (is (> (:recall arts-precision-recall) arts-recall-threshold)))))

(deftest cleanse-commas-test
  (is (=
       (cleanse-commas "art , 24 ust 2 pkt 4")
       "art 24 ust 2 pkt 4"))
  (is (=
       (cleanse-commas "art 24 ust , 2 pkt 3 i 4")
       "art 24 ust 2 pkt 3 i 4")))

(deftest cast-coords-lists-test
  (is (=
       (cast-coords-lists '("1" "2" "0" "0" "0" "0")
                          '("0" "3" "0" "0" "0" "0"))
       '("1" "3" "0" "0" "0" "0")))
  (is (=
       (cast-coords-lists '("1" "2" "0" "0" "2" "2")
                          '("0" "3" "0" "0" "0" "0"))
       '("1" "3" "0" "0" "0" "0")))
  (is (=
       (cast-coords-lists '("1" "2" "0" "0" "2" "2")
                          '("0" "3" "3" "0" "0" "0"))
       '("1" "3" "3" "0" "0" "0")))
  (is (=
       (cast-coords-lists '("1" "2" "0" "0" "2" "2")
                          '("0" "0" "3" "0" "0" "0"))
       '("1" "2" "3" "0" "0" "0"))))

(deftest extract-art-coords-test
  (is (=
       (extract-art-coords "art 90")
       '(("90" "0" "0" "0" "0" "0"))))
  (is (=
       (extract-art-coords "art. 183 ust 5 pkt 2 oraz 6")
       '(("183" "0" "5" "2" "0" "0") ("183" "0" "5" "6" "0" "0"))))
  (is (=
       (extract-art-coords "art. 183 ust 5 pkt 2 oraz ust. 6")
       '(("183" "0" "5" "2" "0" "0") ("183" "0" "6" "0" "0" "0"))))
  (is (=
       (extract-art-coords "art. 89 ust. 1 pkt 2 , pkt 3 , pkt 8")
       '(("89" "0" "1" "2" "0" "0")
         ("89" "0" "1" "3" "0" "0")
         ("89" "0" "1" "8" "0" "0"))))
  (is (=
       (extract-art-coords " art. 89 ust. 1 pkt 2 oraz pkt 4")
       '(("89" "0" "1" "2" "0" "0") ("89" "0" "1" "4" "0" "0"))))
  (is (=
       (extract-art-coords "art. 24 ust. 1 pkt 10 oraz ust. 2 pkt 2")
       '(("24" "0" "1" "10" "0" "0") ("24" "0" "2" "2" "0" "0"))))
  (is (=
       (extract-art-coords "art. 89 ust. 1 pkt. 2 i pkt. 6")
       '(("89" "0" "1" "2" "0" "0") ("89" "0" "1" "6" "0" "0")))))

(deftest get-year-of-law-act-test
  (is (=
       (get-year-of-law-act
        (str
         " ustawy Prawo zamówień publicznych ( Dz. U. t.j. z 2007 r. Nr 223"
         " , poz. 1655 ). O kosztach postępowania orzeczono na podstawie"))
       "2007"))
  (is (=
       (get-year-of-law-act
        (str
         "rozporządzenia Prezesa Rady Ministrów z dnia 17 maja 2006"
         " w sprawie wysokości oraz szczegółowych zasad pobierania"
         " wpisu od odwołania oraz szczegółowych zasad rozliczania"
         " kosztów w postępowaniu odwoławczym ( Dz. U. Nr 87 , poz. 608 )"))
       "2006"))
  (is (=
    "1992"
    (get-year-of-law-act
      (str
        "KONSTYTUCYJNE utrzymane w mocy na podstawie art. 77"
        " Ustawy Konstytucyjnej"
        " z dnia 17 października 1992 r. o wzajemnych stosunkach między"
        " władzą ustawodawczą i wykonawczą Rzeczypospolitej Polskiej "
        "oraz o samorządzie terytorialnym "
        "(Dz. U. Nr 84, poz. 426, z 1995 r. Nr 38, poz. 184): "
        "(uchylony) ogólnie – w. 6.01.09, SK 22/06 (poz. 1), w. 15.01.09"))))
  (is (=
    "1994"
    (get-year-of-law-act
      (str
        " Karta Samorządu Lokalnego sporządzona w Strasburgu"
        " dnia 15 października 1985 r. (Dz. U. z 1994 r. Nr 124, poz. 607"
        " oraz z 2006 r. Nr 154, poz. 1107): art. 4 ust. 2 i 6 "
        "– p. 21.01.09, P 14/08 (poz. 7)"))))
  (is (=
    "1994"
    (get-year-of-law-act
      (str
        " ustawy z dnia 28 grudnia 1989 r. – Prawo celne"
        " (tekst jednolity z 1994 r. Dz.U. Nr 71, poz. 312 ze zm.)"))))
  (is (=
    "1991"
    (get-year-of-law-act
      (str
        "ustawy z dnia 30 sierpnia 1991 r. o zakładach opieki zdrowotnej"
       " (Dz.U. Nr 91, poz. 408 ze zm.) kjhkjh "
       "(Dz.U. z 2001 r. Nr 65, poz. 659)")))))
