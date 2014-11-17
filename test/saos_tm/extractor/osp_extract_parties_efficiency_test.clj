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
          _ (spit "extracted-parties.txt" extracted-parties)
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
          _ (spit "answers.txt" answers)
          precision-recall (get-precision-recall extracted-parties answers)
          precision (precision-recall :precision)
          recall (precision-recall :recall)
        ]
    (is (> precision 0.54))
    (is (> recall 0.46))))
