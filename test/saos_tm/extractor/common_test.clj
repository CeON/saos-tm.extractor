(ns saos-tm.extractor.common-test
  (:require
   [clojure.test :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure-csv.core :as csv]
   [clojure.math.numeric-tower :as numeric-tower]
   [saos-tm.extractor.common :as common]
   [saos-tm.extractor.law-links :as law-links]
   [saos-tm.extractor.judgment-links :as judgment-links]))

(def test-data-path "test-data/")

(def ^:private links-input-txts-dir-name "links-input-txts/")

(def log-data-path "log/")

(defn mkdir-path [path]
  (let [dir (java.io.File. path)]
    (if-not (.exists dir)
      (.mkdirs dir))))

(defn ^:private get-art-coords-csv [art-coords]
  (let [
        art-nr (:art art-coords)
        par-nr (:par art-coords)
        ust-nr (:ust art-coords)
        pkt-nr (:pkt art-coords)
        zd-nr  (:zd art-coords)
        lit-nr (:lit art-coords)
        ]
    (apply str
           "\"" art-nr "\"" common/csv-delimiter
           "\"" par-nr "\"" common/csv-delimiter
           "\"" ust-nr "\"" common/csv-delimiter
           "\"" pkt-nr "\"" common/csv-delimiter
           "\"" zd-nr "\"" common/csv-delimiter
           "\"" lit-nr "\"" common/csv-delimiter)))

(defn get-csv-for-extracted-link [link case-nmb]
  (let [
        art (:art link)
        act (:act link)
        ]
    (apply str (get-art-coords-csv art)
           "\"" case-nmb "\"" common/csv-delimiter
           "\"" (:journalYear act) "\"" common/csv-delimiter
           "\"" (:journalNo act) "\"" common/csv-delimiter
           "\"" (:journalEntry act) "\"" common/system-newline)))

(defn ^:private get-measure [true-positives-count elements-count]
  (when-not
    (= elements-count 0)
    (float (/ true-positives-count elements-count))))

(defn get-precision-recall [extracted-set benchmark-set]
  (if (and (empty? extracted-set) (empty? benchmark-set))
    (zipmap [:precision :recall] [1.0 1.0])
    (let [
          true-positives-count
            (count
             (set/intersection extracted-set benchmark-set))
          extracted-count (count extracted-set)
          benchmark-count (count benchmark-set)
          precision (get-measure true-positives-count extracted-count)
          recall (get-measure true-positives-count benchmark-count)
          ]
      (zipmap [:precision :recall] [precision recall]))))

(defn ^:private read-law-links-to-maps [file-data]
  (let [
        data (csv/parse-csv file-data)
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
  (map read-law-links-to-maps files))

(defn ^:private law-links-extract [txt-files extract-law-links-fn]
  (map
   #(into #{} (:extracted-links (extract-law-links-fn %)))
   txt-files))

(defn judgment-links-extract [txt-files]
  (map
    #(judgment-links/extract-judgment-links %)
    txt-files))

(defn case-nmb-to-csv [case-nmb not-used]
  (apply str "\"" case-nmb "\"" common/system-newline))

(defn law-links-extract-all-dictionaries [txt-files]
  (law-links-extract
   txt-files #(law-links/extract-law-links % true true true)))

(defn ^:private get-average [coll]
  (/ (reduce + coll) (count coll)))

(defn ^:private get-elements [key-name coll]
  (map
   #(key-name %)
   coll))

(defn ^:private get-case-nmb [file-data]
  (map first
       (csv/parse-csv file-data)))

(defn get-benchmark-case-nmbs [ext-files]
  (let [
        jdg-case-nmbs (map get-case-nmb ext-files)
        benchmark-case-nmbs
          (map
           #(set (remove empty? (map str/trim %)))
           jdg-case-nmbs)
        ]
    benchmark-case-nmbs))

(defn list-file-paths [dir]
  (sort (.listFiles (io/file dir))))

(defn ^:private list-file-names [dir]
  (sort (.list (io/file dir))))

(defn ^:private get-files-from-dir [dir]
  (map slurp (sort (list-file-paths dir))))

(defn ^:private map-fn [func coll additional-item]
  (map
   #(func % additional-item)
   coll))

(defn ^:private spit-all-csv [result-to-csv-fn path data]
  (spit path
        (apply str
               (sort
                (map-fn result-to-csv-fn data "case-nmb")))))

(defn ^:private nils-to-zeros [coll]
  (map #(if (nil? %) 0 %) coll))

(defn ^:private zip-with-file-name [file-name coll]
  (map
   #(zipmap
     [:fileName :link]
     [file-name %])
   coll))

(defn ^:private get-items-with-file-names [file-names items]
  (set
   (mapcat zip-with-file-name file-names items)))

(defn ^:private spit-all-csv-with-case-nmbs
  [result-to-csv-fn path data case-nmb]
  (spit path
        (apply str
               (sort
                (map-fn result-to-csv-fn data case-nmb)))))

(defn ^:private split-csv-line [s]
  (str/split s
             (re-pattern (str #"\"" common/csv-delimiter "\""))))

(defn ^:private extract-case-nmbs-from-csv [txts]
  (map
   #(nth (split-csv-line %) 6)
   txts))

(defn log-results-with-case-nmbs
  [result-to-csv-fn log-files-paths extracted-items ext-files]
  (doall
   (map
    #(spit-all-csv-with-case-nmbs result-to-csv-fn %1 %2 %3)
    log-files-paths
    extracted-items
    (extract-case-nmbs-from-csv ext-files))))

(defn log-results-without-case-nmbs
  [result-to-csv-fn log-files-paths extracted-items ext-files]
  (doall
   (map
    #(spit-all-csv result-to-csv-fn %1 %2)
    log-files-paths
    extracted-items)))

(defn remove-page-nmbs [s]
  (str/replace s
               (re-pattern
                (str common/system-newline "\\d+" common/system-newline))
               "\n"))

(defn links-preprocess [coll]
  (map remove-page-nmbs coll))

(defn expand-str-to-length [s length]
  (str s
       (apply str
              (take
               (- length (count s))
               (repeat " ")))))

(defn ^:private format-numbers [coll]
  (map #(format "%.4f" %) coll))

(defn get-harmonic-mean [nmb1 nmb2]
  (if (= 0.0 (+ nmb1 nmb2))
    0.0
    (/ (* 2 nmb1 nmb2) (+ nmb1 nmb2))))

(defn ^:private count-items-from-measures [items measures]
  (map
   #(numeric-tower/round (* (count %1) (- 1.0 %2)))
   items measures))

(defn ^:private get-and-log-efficiencies
  [benchmark-items extracted-items
   ext-files ext-files-names
   log-per-doc-stats-path log-files-paths
   log-results-fn result-to-csv-fn
   per-doc-stats-file-name]
  (let [
        file-names
          (map
           #(first (str/split % #"\."))
           ext-files-names)
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
        counts-precs (count-items-from-measures extracted-items precisions)
        counts-recalls (count-items-from-measures benchmark-items recalls)

        names-precs-recalls
          (sort
           #(compare (nth %1 4) (nth %2 4))
           (map
            vector
            ext-files-names precisions recalls counts-precs counts-recalls))

        files-names-lengths (map #(count (first %)) names-precs-recalls)
        max-file-name-length (apply max files-names-lengths)
        file-names-formatted
          (map
            #(apply str (expand-str-to-length (first %) max-file-name-length))
            names-precs-recalls)

        per-doc-stats-file-description
          (str "\"file name\"" common/csv-delimiter
               "\"precision\"" common/csv-delimiter
               "\"recall\"" common/csv-delimiter
               "\"number of items incorrectly extracted\""
               common/csv-delimiter
               "\"number of items not extracted\"" common/system-newline)
        per-doc-stats-file-content
          (apply str
                 (map
                  #(str %1 common/csv-delimiter " "
                        (str/join (str common/csv-delimiter " ")
                                  (concat
                                   (format-numbers (take 2 (drop 1 %2)))
                                   (drop 3 %2)))
                        common/system-newline)
                   file-names-formatted names-precs-recalls))

        _ (spit
           (str log-per-doc-stats-path "-" per-doc-stats-file-name ".txt")
           (str per-doc-stats-file-description per-doc-stats-file-content))
        _
          (log-results-fn
           result-to-csv-fn log-files-paths extracted-items ext-files)

        _ (print (str "precision: "
                      (format "%.4f" (:precision overall-precision-recall))
                      " recall: "
                      (format "%.4f" (:recall overall-precision-recall))
                      " f1: "
                      (format "%.4f"
                              (get-harmonic-mean
                               (:precision overall-precision-recall)
                               (:recall overall-precision-recall)))
                      common/system-newline))
        ]
    overall-precision-recall))

(defn ^:private prepare-files-and-file-paths
  [links-type-dir-name ext-dir-name txt-dir-name txt-files-conv-fn]
  (let [
        ext-dir-path (str test-data-path links-type-dir-name ext-dir-name)
        ext-files (get-files-from-dir ext-dir-path)
        txt-files
          (txt-files-conv-fn
           (get-files-from-dir
            (str test-data-path links-input-txts-dir-name txt-dir-name "/")))
        ext-files-names (list-file-names ext-dir-path)
        log-files-paths
          (sort
           (map
            #(str log-data-path links-type-dir-name ext-dir-name "/" %)
            ext-files-names))
        _ (mkdir-path
            (str log-data-path links-type-dir-name ext-dir-name "/"))
        ]
    {:ext-files ext-files
     :ext-files-names ext-files-names
     :txt-files txt-files
     :log-files-paths log-files-paths}))

(defn judgment-links-efficiency-test
  [links-type-dir-name txt-dir-name ext-dir-name
   benchmark-records-fn extracted-records-fn txt-files-conv-fn
   precision-threshold recall-threshold result-to-csv-fn log-results-fn]
  (let [
        files-and-file-paths
          (prepare-files-and-file-paths
           links-type-dir-name ext-dir-name txt-dir-name txt-files-conv-fn)

        benchmark-items
          (benchmark-records-fn (files-and-file-paths :ext-files))
        extracted-items
          (extracted-records-fn (files-and-file-paths :txt-files))

        _ (println)
        overall-precision-recall
          (get-and-log-efficiencies
           benchmark-items extracted-items
           (files-and-file-paths :ext-files)
           (files-and-file-paths :ext-files-names)
           (str log-data-path links-type-dir-name ext-dir-name)
           (files-and-file-paths :log-files-paths)
           log-results-fn result-to-csv-fn
           "per-doc-stats")
        ]
    (is (> (:precision overall-precision-recall) precision-threshold))
    (is (> (:recall overall-precision-recall) recall-threshold))))

(defn ^:private extract-elems [key-name coll]
  (set
   (map #(key-name %) coll)))

(defn law-links-efficiency-test
  [links-type-dir-name txt-dir-name ext-dir-name
   benchmark-records-fn extracted-records-fn txt-files-conv-fn
   acts-precision-threshold acts-recall-threshold
   arts-precision-threshold arts-recall-threshold
   overall-precision-threshold overall-recall-threshold
   result-to-csv-fn log-results-fn]
  (let [
        files-and-file-paths
          (prepare-files-and-file-paths
           links-type-dir-name ext-dir-name txt-dir-name txt-files-conv-fn)

        benchmark-items
          (benchmark-records-fn (files-and-file-paths :ext-files))
        benchmark-acts (map #(extract-elems :act %) benchmark-items)
        benchmark-arts (map #(extract-elems :art %) benchmark-items)

        extracted-items
          (extracted-records-fn (files-and-file-paths :txt-files))
        extracted-acts (map #(extract-elems :act %) extracted-items)
        extracted-arts (map #(extract-elems :art %) extracted-items)

        _ (println)
        _ (print (str (expand-str-to-length "ACTS " 15) "| "))
        acts-precision-recall
          (get-and-log-efficiencies
           benchmark-acts extracted-acts
           (files-and-file-paths :ext-files)
           (files-and-file-paths :ext-files-names)
           (str log-data-path links-type-dir-name ext-dir-name)
           (files-and-file-paths :log-files-paths)
           log-results-fn result-to-csv-fn
           "ACTS-per-doc-stats")

        _ (print (str (expand-str-to-length "ARTS " 15) "| "))
        arts-precision-recall
          (get-and-log-efficiencies
           benchmark-arts extracted-arts
           (files-and-file-paths :ext-files)
           (files-and-file-paths :ext-files-names)
           (str log-data-path links-type-dir-name ext-dir-name)
           (files-and-file-paths :log-files-paths)
           log-results-fn result-to-csv-fn
           "ARTS-per-doc-stats")

        _ (print (str (expand-str-to-length "ACTS-ARTS " 15) "| "))
        overall-precision-recall
          (get-and-log-efficiencies
           benchmark-items extracted-items
           (files-and-file-paths :ext-files)
           (files-and-file-paths :ext-files-names)
           (str log-data-path links-type-dir-name ext-dir-name)
           (files-and-file-paths :log-files-paths)
           log-results-fn result-to-csv-fn
           "ACTS-ARTS-per-doc-stats")
        ]
    (is (> (:precision overall-precision-recall) overall-precision-threshold))
    (is (> (:recall overall-precision-recall)    overall-recall-threshold))
    (is (> (:precision acts-precision-recall)    acts-precision-threshold))
    (is (> (:recall acts-precision-recall)       acts-recall-threshold))
    (is (> (:precision arts-precision-recall)    arts-precision-threshold))
    (is (> (:recall arts-precision-recall)       arts-recall-threshold))))

; tests

(deftest get-harmonic-mean-test
  (is (= 0.0 (get-harmonic-mean 0.0 0.0)))
  (is (<
       (numeric-tower/abs (- (get-harmonic-mean 0.9 0.6) 0.72))
       1E-10)))

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
  (is (=
       (#'saos-tm.extractor.common/unsplit-words-across-lines
        "postę-\npowania")
       "postępowania")))

(deftest preprocess-test
  (is (= (common/preprocess "postę-\npowania") "postępowania")))
