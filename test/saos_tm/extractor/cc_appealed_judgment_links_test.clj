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

(deftest remove-html-tags-other-than-span-test []
  (is (=
       (common/remove-html-tags-other-than-span
        (str "<p></p>kdjh <span> </span>"
             " <span class=\"anon-block\"> (...) S.A.</span>"))
       (str "  kdjh <span> </span> "
            "<span class=\"anon-block\"> (...) S.A.</span>"))))

(defn appeals-to-write [coll]
  (str/join common/system-newline (sort coll)))

(defn results-to-file [elements1 elements2 results-type court-type]
  (if (not (.isDirectory (io/file common-test/log-data-path)))
    (.mkdir (io/file common-test/log-data-path)))
  (let [
        results (set/difference elements1 elements2)
        _ (spit
           (str common-test/log-data-path court-type "-" results-type ".txt")
           (appeals-to-write results))
        ]))

(defn results-to-strs [file-names extracted-appeals]
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

(defn handle-appeal-test [court-type extract-fn]
  (let [
        file-paths
          (common-test/list-file-paths
           (str "test-data/cc-appealed/" court-type "/"))
        file-names (map #(last (str/split (str %) #"/")) file-paths)
        sentences (map #(slurp %) file-paths)
        extracted-appeals
          (map #(extract-fn %) sentences)
        extracted-appeals-strs
          (results-to-strs file-names extracted-appeals)
        correct-appeals-strs
          (into #{}
                (common-test/split-lines
                 (slurp
                  (str "test-data/cc-appealed/" court-type ".txt"))))

        _
          (results-to-file
           correct-appeals-strs extracted-appeals-strs
           "correct" court-type)
        _
          (results-to-file
           extracted-appeals-strs correct-appeals-strs
           "error" court-type)

        precision-recall
          (common-test/get-precision-recall
           extracted-appeals-strs correct-appeals-strs)
        ]
    precision-recall))

(deftest extract-appeals-efficiency-test []
  (let [
        court-types
          ["app-sentence" "reg-sentence"
           "app-decision" "reg-decision"
           "app-decision-complaint" "reg-decision-complaint"]
        extract-fns
          [cc-appealed-judgment-links/extract-appeal-or-grievance
           cc-appealed-judgment-links/extract-appeal-or-grievance
           cc-appealed-judgment-links/extract-appeal-or-grievance
           cc-appealed-judgment-links/extract-appeal-or-grievance
           cc-appealed-judgment-links/extract-complaint
           cc-appealed-judgment-links/extract-complaint]

        results
          (map
           #(handle-appeal-test %1 %2)
           court-types extract-fns)
        _ (prn results)
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
