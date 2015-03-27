(ns saos-tm.extractor.cc-parties-efficiency-test
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :refer :all]
    [langlab.core.parsers :refer :all]
    [saos-tm.extractor.common :refer :all]
    [saos-tm.extractor.common-test :refer :all]
    [saos-tm.extractor.cc-parties :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn extract-parties-from-judgments [judgments extract-parties-func]
  (remove nil?
          (map
           #(extract-parties-func %)
           judgments)))

(defn split-csv [s]
  (str/split
   s
   (re-pattern (str "\"" csv-delimiter "\""))))

(defn answers-to-string [coll]
  (map #(str  (:id %) system-newline
              (:plaintiff %) system-newline
              (:defendant %) system-newline)
       coll))

(def cc-parties-test-data-path "test-data/cc-parties/")

(defn remove-opening-closing-quots [coll]
  (map
   #(subs % 1 (dec (count %)))
   coll))

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

(defn extract-parties [coll]
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

(defn cc-parties-paths [file-names]
  (map
   #(str cc-parties-test-data-path %)
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
        files-funcs
          {:test-sets ["answers-1/civil/" "answers-1/criminal/"
                       "answers-2/civil/" "answers-2/criminal/"
                       "answers-3/civil/" "answers-3/criminal/"]
           :answers  ["answers-1-civil.txt" "answers-1-criminal.txt"
                      "answers-2-civil.txt" "answers-2-criminal.txt"
                      "answers-3-civil.txt" "answers-3-criminal.txt"]
           :corrects ["log/corrects-1-civil.txt" "log/corrects-1-criminal.txt"
                      "log/corrects-2-civil.txt" "log/corrects-2-criminal.txt"
                      "log/corrects-3-civil.txt" "log/corrects-3-criminal.txt"]
           :errors    ["log/errors-1-civil.txt" "log/errors-1-criminal.txt"
                       "log/errors-2-civil.txt" "log/errors-2-criminal.txt"
                       "log/errors-3-civil.txt" "log/errors-3-criminal.txt"]
           :extract-parties-funcs
             [extract-parties-cc-civil extract-parties-cc-criminal
              extract-parties-cc-civil extract-parties-cc-criminal
              extract-parties-cc-civil extract-parties-cc-criminal]
           }

        judgments
          (map
            #(get-file-contents (str cc-parties-test-data-path %) #"[\s\S]*")
             (files-funcs :test-sets))

        ids
          (map
            #(get-file-names (str cc-parties-test-data-path %) #"[\s\S]*")
             (files-funcs :test-sets))

        extracted-parties
          (map #(extract-parties-from-judgments %1 %2)
               judgments
               (files-funcs :extract-parties-funcs))
        extracted-parties-with-ids
          (map #(join-with-ids %1 %2) extracted-parties ids)
        extracted-parties-with-ids-sets
          (map #(into #{} %) extracted-parties-with-ids)

        answers-txts
         (map #(slurp %) (cc-parties-paths (files-funcs :answers)))
        answers-lines (map #(split-lines %) answers-txts)
        answers-without-quots
          (map #(remove-opening-closing-quots %) answers-lines)
        is-civil [true false true false true false]
        answers
          (map #(create-cc-parties-map %1 %2)
               answers-without-quots
               is-civil)

        corrects
          (map #(difference %1 %2)
               answers extracted-parties-with-ids-sets)
        _ (handle-results corrects (files-funcs :corrects))

        errors
          (map #(difference %1 %2) extracted-parties-with-ids-sets answers)
        _ (handle-results errors (files-funcs :errors))

        precisions-recalls
          (get-precisions-recalls extracted-parties-with-ids-sets answers)
        efficiencies (map #(:recall %) precisions-recalls)

        _
          (doall
           (map
            #(prn (str %1 %2 %3))
            (files-funcs :test-sets)
            (repeat ", efficiency: ")
            efficiencies))
        ]
    (is (= ((nth precisions-recalls 0) :recall)    1.0    ))
    (is (= ((nth precisions-recalls 1) :recall)    1.0    ))
    (is (= ((nth precisions-recalls 2) :recall)    1.0  ))
    (is (> ((nth precisions-recalls 3) :recall)    0.956  ))
    (is (> ((nth precisions-recalls 4) :recall)    0.958  ))
    (is (= ((nth precisions-recalls 5) :recall)    1.0  ))))
