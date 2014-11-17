(ns saos-tm.extractor.osp-extract-parties-efficiency-test
  (:require
    [clojure.test :refer :all]
    [ clojure.string :as str ]
    [ clojure.set :refer :all ]
    [ langlab.core.parsers :refer :all ]
    [saos-tm.extractor.common :refer :all]
    [saos-tm.extractor.osp-extract-parties :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn split-csv [s]
  (str/split
   s
   (re-pattern (str "\"" csv-delimiter "\""))))

(defn answers-to-string [coll]
  (map #(str  (:id %) system-newline
              (:plaintiff %) system-newline
              (:defendant %) system-newline)
       coll))

(deftest extract-parties-efficiency-test []
  (let [
          judgments (get-judgments [test-set-xml-path])
          extracted-parties
            (into #{}
              (mapcat
                #(extract-parties-from-judgments %)
               judgments))
          answers-txt (slurp "test-data/osp-parties/answers.txt")
          answers-lines
            (str/split
             answers-txt
             (re-pattern system-newline))
          answers-without-quots
            (map
             #(subs % 1 (dec (count %)))
             answers-lines)
          answers
            (into #{}
              (map
               #(zipmap [:id :plaintiff :defendant] (split-csv %))
               answers-without-quots))
          correct (sort-by #(:id %) (difference answers extracted-parties))
          correct (answers-to-string correct)
          errors (sort-by #(:id %) (difference extracted-parties answers))
          errors (answers-to-string errors)
          ;_ (spit "correct.txt" (apply str correct))
          ;_ (spit "errors.txt" (apply str errors))
          precision-recall (get-precision-recall extracted-parties answers)
          precision (precision-recall :precision)
          recall (precision-recall :recall)
          _ (prn (str "precision: " precision " recall: " recall))
        ]
    (is (> precision 0.94))
    (is (> recall 0.94))))
