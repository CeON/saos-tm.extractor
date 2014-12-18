(ns saos-tm.extractor.cc-appealed-judgment-links-test
  (:require
   [clojure.test :refer :all]
   [ clojure.string :as str ]
   [clojure.set :refer :all]
   [saos-tm.extractor.common :refer :all]
   [saos-tm.extractor.common-test :refer :all]
   [saos-tm.extractor.cc-appealed-judgment-links :refer :all]
   [clojure.java.io :as io]))

(deftest remove-html-tags-other-than-span-test []
  (is (=
       (remove-html-tags-other-than-span
        (str "<p></p>kdjh <span> </span>"
             " <span class=\"anon-block\"> (...) S.A.</span>"))
        "  kdjh <span> </span> <span class=\"anon-block\"> (...) S.A.</span>")))

(defn appeals-to-write [coll]
  (str/join system-newline (sort coll)))

(defn results-to-file [elements1 elements2 results-type court-type]
  (let [
        log-path "log/"
        ]
    (if (not (.isDirectory (io/file log-path)))
      (.mkdir (io/file log-path)))
       (let [
             results (difference elements1 elements2)
             _ (spit (str log-path court-type "-" results-type ".txt")
                     (appeals-to-write results))
             ])))

(defn results-to-strs [file-names extracted-appeals]
  (into #{}
        (map
         #(str "\"" %1 "\"" csv-delimiter "\""
               (:appeal-type %2) "\"" csv-delimiter "\""
               (:appellant %2) "\"" csv-delimiter "\""
               (:judgment-type %2) "\"" csv-delimiter "\""
               (:court %2) "\"" csv-delimiter "\""
               (:date %2) "\"" csv-delimiter "\""
               (:signature %2) "\"")
         file-names extracted-appeals)))

(defn handle-appeal-test [court-type extract-fn]
  (let [
        file-paths
          (list-file-paths
           (str "test-data/cc-appealed/" court-type "/"))
        file-names (map #(last (str/split (str %) #"/")) file-paths)
        sentences (map #(slurp %) file-paths)
        extracted-appeals
          (map #(extract-fn %) sentences)
        extracted-appeals-strs
          (results-to-strs file-names extracted-appeals)
        correct-appeals-strs
          (into #{}
                (split-lines
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
          (get-precision-recall extracted-appeals-strs correct-appeals-strs)
        ]
    precision-recall))

(deftest extract-appeals-efficiency-test []
  (let [
        court-types
          ["app-sentence" "reg-sentence"
           "app-decision" "reg-decision"
           "app-decision-complaint" "reg-decision-complaint"]
        extract-fns
          [extract-appeal-or-grievance
           extract-appeal-or-grievance
           extract-appeal-or-grievance
           extract-appeal-or-grievance
           extract-complaint
           extract-complaint]

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
    (is (> (:recall    (nth results 2)) 0.967))
    (is (> (:precision (nth results 2)) 0.967))
    (is (> (:recall    (nth results 3)) 0.701))
    (is (> (:precision (nth results 3)) 0.734))
    (is (> (:recall    (nth results 4)) 0.33))
    (is (> (:precision (nth results 4)) 0.33))
    (is (> (:recall    (nth results 5)) -0.01))
    (is (> (:precision (nth results 5)) -0.01))))
