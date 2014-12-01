(ns saos-tm.extractor.osp-extract-parties-efficiency-test
  (:require
    [clojure.test :refer :all]
    [ clojure.string :as str ]
    [ clojure.set :refer :all ]
    [ langlab.core.parsers :refer :all ]
    [saos-tm.extractor.common :refer :all]
    [saos-tm.extractor.common-test :refer :all]
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

(def osp-parties-test-data-path "test-data/osp-parties/")

(defn remove-opening-closing-quots [coll]
  (map
   #(subs % 1 (dec (count %)))
   coll))

(defn create-osp-parties-map [coll]
  (into #{}
        (map
         #(zipmap [:id :plaintiff :defendant] (split-csv %))
         coll)))

(defn extract-parties [coll]
  (prn "extract-parties")
  (prn (count coll))
  (into #{}
        (mapcat
         #(extract-parties-from-judgments %)
         coll)))

(defn get-elements-sorted-by-id [elements]
  (sort-by
   #(:id %)
   elements))

(defn spit-many [paths coll]
  (doall
   (map
    #(spit %1 (apply str %2))
    paths
    coll)))

(defn handle-results [results paths]
  (let [
        results-sorted (map #(get-elements-sorted-by-id %) results)
        results-strs (map #(answers-to-string %) results-sorted)
        _ (spit-many paths results-strs)
        ]))

(defn osp-parties-paths [file-names]
  (map
   #(str osp-parties-test-data-path %)
   file-names))

(deftest extract-parties-efficiency-test []
  (let [
          file-names
            {:test-sets ["test-set-1.xml" "test-set-2.xml" "test-set-3.xml"]
             :answers   ["answers-1.txt"  "answers-2.txt" "answers-3.txt"]
             :corrects  ["corrects-1.txt" "corrects-2.txt" "corrects-3.txt"]
             :errors    ["errors-1.txt"   "errors-2.txt" "errors-3.txt"]}

          judgments (get-judgments (osp-parties-paths (file-names :test-sets)))
          extracted-parties
            (map
             #(into #{} (extract-parties-from-judgments %))
             judgments)

          answers-txts
            (map #(slurp %) (osp-parties-paths (file-names :answers)))
          answers-lines
            (map
             #(str/split
             %
             (re-pattern system-newline))
             answers-txts)
          answers-without-quots
            (map #(remove-opening-closing-quots %) answers-lines)
          answers (map #(create-osp-parties-map %) answers-without-quots)

          corrects (map #(difference %1 %2) answers extracted-parties)
          _ (handle-results corrects (file-names :corrects))

          errors (map #(difference %1 %2) extracted-parties answers)
          _ (handle-results errors (file-names :errors))

          precisions-recalls (get-precisions-recalls extracted-parties answers)
          _ (prn precisions-recalls)
        ]
    (is (> ((nth precisions-recalls 0) :recall) 0.977))
    (is (> ((nth precisions-recalls 0) :precision) 0.977))
    (is (> ((nth precisions-recalls 1) :recall) 0.979))
    (is (> ((nth precisions-recalls 1) :precision) 0.989))
    (is (> ((nth precisions-recalls 2) :recall) 0.809))
    (is (> ((nth precisions-recalls 2) :precision) 0.835))))
