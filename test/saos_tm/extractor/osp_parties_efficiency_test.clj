(ns saos-tm.extractor.osp-parties-efficiency-test
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :refer :all]
    [langlab.core.parsers :refer :all]
    [saos-tm.extractor.common :refer :all]
    [saos-tm.extractor.common-test :refer :all]
    [saos-tm.extractor.osp-parties :refer :all])
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

(defn join-with-ids [extracted-parties ids]
  (map
   #(zipmap [:defendant :plaintiff :id]
            [(%1 :defendant) (%1 :plaintiff) %2])
   extracted-parties ids))

(deftest extract-parties-efficiency-test []
  (let [
        files
          {:test-sets ["answers-1/"
                       "answers-2/"
                       "answers-3/"
                       ]
           :answers   ["answers-1.txt"
                       "answers-2.txt"
                       "answers-3.txt"
                       ]
           :corrects  ["corrects-1.txt"
                       "corrects-2.txt"
                       "corrects-3.txt"
                       ]
           :errors    ["errors-1.txt"
                       "errors-2.txt"
                       "errors-3.txt"
                       ]}

        judgments
          (map
            #(get-file-contents (str osp-parties-test-data-path %) #"[\s\S]*")
             (files :test-sets))

        ids
          (map
            #(get-file-names (str osp-parties-test-data-path %) #"[\s\S]*")
             (files :test-sets))

        extracted-parties
          (map #(extract-parties-from-judgments %) judgments)
        extracted-parties-with-ids
          (map #(join-with-ids %1 %2) extracted-parties ids)
        extracted-parties-with-ids
          (map #(into #{} %) extracted-parties-with-ids)

        answers-txts
         (map #(slurp %) (osp-parties-paths (files :answers)))
        answers-lines (map #(split-lines %) answers-txts)
        answers-without-quots
          (map #(remove-opening-closing-quots %) answers-lines)
        answers (map #(create-osp-parties-map %) answers-without-quots)

        corrects
          (map #(difference %1 %2)
               answers extracted-parties-with-ids)
        _ (handle-results corrects (files :corrects))

        errors (map #(difference %1 %2) extracted-parties-with-ids answers)
        _ (handle-results errors (files :errors))

        precisions-recalls
          (get-precisions-recalls extracted-parties-with-ids answers)
        _ (prn precisions-recalls)
        ]
    (is (> ((nth precisions-recalls 0) :recall) 0.977))
    (is (> ((nth precisions-recalls 0) :precision) 0.977))
    (is (> ((nth precisions-recalls 1) :recall) 0.969))
    (is (> ((nth precisions-recalls 1) :precision) 0.969))
    (is (> ((nth precisions-recalls 2) :recall) 0.869))
    (is (> ((nth precisions-recalls 2) :precision) 0.869))
    ))
