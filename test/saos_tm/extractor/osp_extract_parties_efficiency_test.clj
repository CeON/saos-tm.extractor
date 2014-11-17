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
          errors (difference answers extracted-parties)
          errors1 (difference extracted-parties answers)
          are-equal (= errors errors1)
          ;_ (prn are-equal)
          ;_ (prn errors)
          ;_ (prn errors1)
          ;_ (prn (count errors))
          ;_ (prn (count errors1))
          ;_ (prn (count answers))
          ;_ (prn (count extracted-parties))
          precision-recall (get-precision-recall extracted-parties answers)
          precision (precision-recall :precision)
          recall (precision-recall :recall)
          _ (prn (str "precision: " precision " recall: " recall))
        ]
    (is (> precision 0.54))
    (is (> recall 0.46))))
