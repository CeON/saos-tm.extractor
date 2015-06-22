(ns saos-tm.extractor.cc-parties-efficiency-test
  (:require
   [clojure.test :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [saos-tm.extractor.common :as common]
   [saos-tm.extractor.common-test :as common-test]
   [saos-tm.extractor.cc-parties :as cc-parties])
  (:import java.io.File)
  (:gen-class))

(def cc-parties-dir-name "cc-parties/")

(def files-and-funcs
  {:test-sets ["input-1-civil/" "input-1-criminal/"
               "input-2-civil/" "input-2-criminal/"
               "input-3-civil/" "input-3-criminal/"]
   :answers  ["answers-1-civil.txt" "answers-1-criminal.txt"
              "answers-2-civil.txt" "answers-2-criminal.txt"
              "answers-3-civil.txt" "answers-3-criminal.txt"]
   :paths-for-corrects
   (map #(str common-test/log-data-path cc-parties-dir-name %)
   ["corrects-1-civil.txt" "corrects-1-criminal.txt"
    "corrects-2-civil.txt" "corrects-2-criminal.txt"
    "corrects-3-civil.txt" "corrects-3-criminal.txt"])
   :paths-for-errors
   (map #(str common-test/log-data-path cc-parties-dir-name %)
   ["errors-1-civil.txt" "errors-1-criminal.txt"
    "errors-2-civil.txt" "errors-2-criminal.txt"
    "errors-3-civil.txt" "errors-3-criminal.txt"])
   :extract-parties-funcs
   [cc-parties/extract-parties-cc-civil
    cc-parties/extract-parties-cc-criminal
    cc-parties/extract-parties-cc-civil
    cc-parties/extract-parties-cc-criminal
    cc-parties/extract-parties-cc-civil
    cc-parties/extract-parties-cc-criminal]})

; Files utilities

(defn get-file-contents [dir re]
  (let [
        sorted-paths (sort (common/get-file-paths dir re))
        ]
    (map #(slurp %) sorted-paths)))

(defn get-file-names [dir re]
  (let [
        sorted-paths (sort (common/get-file-paths dir re))
        ]
    (map #(last (str/split (str %) #"/")) sorted-paths)))

; CSV utilities

(defn split-csv [s]
  (str/split
   s
   (re-pattern (str "\"" common/csv-delimiter "\""))))

(defn create-cc-parties-map [answers is-civil]
  (into #{}
        (map
         #(if is-civil
            (zipmap
             [:id :plaintiff :defendant]
             (split-csv %))
            (zipmap
             [:id :prosecutor]
             (split-csv %)))
         answers)))

; End of CSV utilities

(defn extract-parties-from-judgments [judgments extract-parties-func]
  (remove nil?
          (map
           #(extract-parties-func %)
           judgments)))

(defn answers-to-string [coll]
  (map #(str  (:id %) common/system-newline
              (:plaintiff %) common/system-newline
              (:defendant %) common/system-newline)
       coll))

(defn remove-opening-closing-quots [coll]
  (map
   #(subs % 1 (dec (count %)))
   coll))

(defn extract-parties [coll]
  (into #{}
        (mapcat
         #(extract-parties-from-judgments %)
         coll)))

(defn get-elements-sorted-by-id [elements]
  (sort-by #(:id %) elements))

(defn spit-many [paths coll]
  (doall
   (map
    #(spit %1 (apply str %2))
    paths
    coll)))

(defn handle-results [results paths]
  (let [
        _ (common-test/mkdir-path
           (str common-test/log-data-path cc-parties-dir-name))
        results-sorted (map #(get-elements-sorted-by-id %) results)
        results-strs (map #(answers-to-string %) results-sorted)
        _ (spit-many paths results-strs)
        ]))

(defn cc-parties-paths [file-names]
  (map
   #(str common-test/test-data-path cc-parties-dir-name %)
   file-names))

(defn join-with-ids [extracted-parties ids]
  (map
   #(if
      (or (contains? %1 :prosecutor) (contains? %1 :plaintiff))
      (assoc %1 :id %2)
      (zipmap [:prosecutor :id] ["" %2]))
   extracted-parties ids))

(deftest extract-parties-efficiency-test []
  (let [
        judgments
          (map
            #(get-file-contents
              (str common-test/test-data-path cc-parties-dir-name %)
              #"[\s\S]*")
             (files-and-funcs :test-sets))

        ids
          (map
            #(get-file-names
              (str common-test/test-data-path cc-parties-dir-name %)
              #"[\s\S]*")
             (files-and-funcs :test-sets))

        extracted-parties
          (map #(extract-parties-from-judgments %1 %2)
               judgments
               (files-and-funcs :extract-parties-funcs))
        extracted-parties-with-ids
          (map #(join-with-ids %1 %2) extracted-parties ids)
        extracted-parties-with-ids-sets
          (map #(into #{} %) extracted-parties-with-ids)

        answers-txts
         (map #(slurp %) (cc-parties-paths (files-and-funcs :answers)))
        answers-lines (map #(common-test/split-lines %) answers-txts)
        answers-without-quots
          (map #(remove-opening-closing-quots %) answers-lines)
        is-civil [true false true false true false]
        answers
          (map #(create-cc-parties-map %1 %2)
               answers-without-quots
               is-civil)

        corrects
          (map #(set/difference %1 %2)
               answers extracted-parties-with-ids-sets)
        _ (handle-results corrects (files-and-funcs :paths-for-corrects))

        errors
          (map #(set/difference %1 %2) extracted-parties-with-ids-sets answers)
        _ (handle-results errors (files-and-funcs :paths-for-errors))

        precisions-recalls
          (map
           common-test/get-precision-recall
           extracted-parties-with-ids-sets answers)
        efficiencies (map #(:recall %) precisions-recalls)

        _
          (doall
           (map
            #(prn (str %1 %2 %3))
            (files-and-funcs :test-sets)
            (repeat ", efficiency: ")
            efficiencies))
        ]
    (is (= ((nth precisions-recalls 0) :recall)    1.0    ))
    (is (= ((nth precisions-recalls 1) :recall)    1.0    ))
    (is (= ((nth precisions-recalls 2) :recall)    1.0    ))
    (is (> ((nth precisions-recalls 3) :recall)    0.956  ))
    (is (> ((nth precisions-recalls 4) :recall)    0.958  ))
    (is (= ((nth precisions-recalls 5) :recall)    1.0  ))))
