(ns saos-tm.extractor.cc-appealed-judgment-links-test
  (:require
   [clojure.test :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [saos-tm.extractor.common :as common]
   [saos-tm.extractor.common-test :as common-test]
   [saos-tm.extractor.cc-appealed-judgment-links
    :as cc-appealed-judgment-links]))

(def ^:private cc-appealed-dir-name "cc-appealed/")

(defn ^:private log-results-to-file [elems court-type]
  (common-test/mkdir-path
   (str common-test/log-data-path cc-appealed-dir-name))
  (let [
        _ (spit
           (str common-test/log-data-path cc-appealed-dir-name
                court-type "-answers.txt")
           (str/join common/system-newline (sort elems)))
        ]))

(defn ^:private results-to-strs [file-names extracted-appeals]
  (into #{}
        (map
         #(str "\"" %1 "\"" common/csv-delimiter "\""
               (:appeal-type %2) "\"" common/csv-delimiter "\""
               (:appellant %2) "\"" common/csv-delimiter "\""
               (:judgment-type %2) "\"" common/csv-delimiter "\""
               (:court %2) "\"" common/csv-delimiter "\""
               (:date %2) "\"" common/csv-delimiter "\""
               (:signature %2) "\"")
         file-names extracted-appeals)))

(defn ^:private handle-appeal-test [court-type extract-fn]
  (let [
        file-paths
          (common-test/list-file-paths
           (str
            common-test/test-data-path cc-appealed-dir-name court-type "/"))
        file-names (map #(last (str/split (str %) #"/")) file-paths)
        sentences (map slurp file-paths)
        extracted-appeals (map extract-fn sentences)
        extracted-appeals-strs (results-to-strs file-names extracted-appeals)
        path-to-answers-file
          (str
           common-test/test-data-path cc-appealed-dir-name
           court-type "-answers.txt")
        correct-appeals-strs
          (into #{}
                (str/split-lines
                 (slurp path-to-answers-file)))

        _ (log-results-to-file extracted-appeals-strs court-type)
        precision-recall
          (common-test/get-precision-recall
           extracted-appeals-strs correct-appeals-strs)
        ]
    precision-recall))

(deftest extract-appeals-efficiency-test []
  (let [
        court-types
          ["appeal-sentence" "regional-sentence"
           "appeal-decision" "regional-decision"
           "appeal-decision-complaint" "regional-decision-complaint"]
        extract-fns
          [cc-appealed-judgment-links/extract-appeal-or-grievance-judgment-link
           cc-appealed-judgment-links/extract-appeal-or-grievance-judgment-link
           cc-appealed-judgment-links/extract-appeal-or-grievance-judgment-link
           cc-appealed-judgment-links/extract-appeal-or-grievance-judgment-link
           cc-appealed-judgment-links/extract-complaint-judgment-link
           cc-appealed-judgment-links/extract-complaint-judgment-link]

        results (map handle-appeal-test court-types extract-fns)

        _ (println)
        _
          (doall
           (map
            #(println
              (str (common-test/expand-str-to-length %1 30)
                   "| precision: " (format "%.4f" (%2 :precision))
                   " recall: " (format "%.4f" (%2 :recall))
                   " f1: " (format "%.4f"
                              (common-test/get-harmonic-mean
                               (%2 :precision) (%2 :recall)))))
            court-types results))
        ]
    (is (= (:recall    (nth results 0)) 1.0))
    (is (= (:precision (nth results 0)) 1.0))
    (is (= (:recall    (nth results 1)) 1.0))
    (is (= (:precision (nth results 1)) 1.0))
    (is (> (:recall    (nth results 2)) 0.983))
    (is (> (:precision (nth results 2)) 0.983))
    (is (> (:recall    (nth results 3)) 0.746))
    (is (> (:precision (nth results 3)) 0.781))
    (is (> (:recall    (nth results 4)) 0.33))
    (is (> (:precision (nth results 4)) 0.33))
    (is (> (:recall    (nth results 5)) -0.01))
    (is (> (:precision (nth results 5)) -0.01))))

(deftest remove-html-tags-other-than-span-test []
  (is (=
       (common/remove-html-tags-other-than-span
        (str "<p></p>kdjh <span> </span>"
             " <span class=\"anon-block\"> (...) S.A.</span>"))
       (str "  kdjh <span> </span> "
            "<span class=\"anon-block\"> (...) S.A.</span>"))))
