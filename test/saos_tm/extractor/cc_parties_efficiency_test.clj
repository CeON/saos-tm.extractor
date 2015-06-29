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

(def ^:private cc-parties-dir-name "cc-parties/")

(def ^:private files-and-funcs
  {:test-input-txts-dirs-paths
   ["input-1-civil/" "input-1-criminal/"
    "input-2-civil/" "input-2-criminal/"
    "input-3-civil/" "input-3-criminal/"]
   :answers-files-names
   ["answers-1-civil.txt" "answers-1-criminal.txt"
    "answers-2-civil.txt" "answers-2-criminal.txt"
    "answers-3-civil.txt" "answers-3-criminal.txt"]
   :extract-parties-funcs
   [cc-parties/extract-cc-parties-civil
    cc-parties/extract-cc-parties-criminal
    cc-parties/extract-cc-parties-civil
    cc-parties/extract-cc-parties-criminal
    cc-parties/extract-cc-parties-civil
    cc-parties/extract-cc-parties-criminal]})

(def ^:private is-civil [true false true false true false])

; Files utilities

(defn ^:private get-file-contents [dir re]
  (let [
        sorted-paths (sort (common/get-file-paths dir re))
        ]
    (map slurp sorted-paths)))

(defn ^:private get-file-names [dir re]
  (let [
        sorted-paths (sort (common/get-file-paths dir re))
        ]
    (map
     #(last (str/split (str %) #"/"))
     sorted-paths)))

; CSV utilities

(defn ^:private split-csv [s]
  (str/split
   s
   (re-pattern (str "\"" common/csv-delimiter "\""))))

(defn ^:private create-cc-parties-map [answers is-civil]
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

(defn ^:private extract-parties-from-judgments [judgments extract-parties-func]
  (remove nil?
          (map
           #(extract-parties-func %)
           judgments)))

(defn ^:private answers-to-string [coll]
  (map #(apply str
         "\"" (:id %) "\"" common/csv-delimiter
         "\"" (:plaintiff %) "\"" common/csv-delimiter
         "\"" (:defendant %) "\"" common/system-newline)
       coll))

(defn ^:private remove-opening-closing-quots [coll]
  (map
   #(subs % 1 (dec (count %)))
   coll))

(defn ^:private spit-many [paths coll]
  (doall
   (map
    #(spit %1 (apply str (sort %2)))
    paths
    coll)))

(defn ^:private log-results-to-files [results paths]
  (common-test/mkdir-path
   (str common-test/log-data-path cc-parties-dir-name))
  (let [
        results-strs (map answers-to-string results)
        _ (spit-many paths results-strs)
        ]))

(defn ^:private create-cc-parties-paths [file-names]
  (map
   #(str common-test/test-data-path cc-parties-dir-name %)
   file-names))

(defn ^:private join-with-ids [extracted-parties ids]
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
             (files-and-funcs :test-input-txts-dirs-paths))
        ids
          (map
            #(get-file-names
              (str common-test/test-data-path cc-parties-dir-name %)
              #"[\s\S]*")
             (files-and-funcs :test-input-txts-dirs-paths))

        extracted-parties
          (map extract-parties-from-judgments
               judgments (files-and-funcs :extract-parties-funcs))
        extracted-parties-with-ids (map join-with-ids extracted-parties ids)
        extracted-parties-with-ids-sets
          (map #(into #{} %) extracted-parties-with-ids)

        answers-txts
         (map slurp
              (create-cc-parties-paths (files-and-funcs :answers-files-names)))
        answers-lines (map str/split-lines answers-txts)
        answers-without-quots
          (map remove-opening-closing-quots answers-lines)
        answers (map create-cc-parties-map answers-without-quots is-civil)

        paths-for-logging
          (map
           #(str common-test/log-data-path cc-parties-dir-name %)
           (files-and-funcs :answers-files-names))
        _ (log-results-to-files extracted-parties-with-ids paths-for-logging)

        precisions-recalls
          (map
           common-test/get-precision-recall
           extracted-parties-with-ids-sets answers)
        efficiencies (map #(:recall %) precisions-recalls)

        _ (println)
        _
          (doall
           (map
            #(println
              (str (common-test/expand-str-to-length %1 20)
                   %2 (format "%.4f" %3)))
            (files-and-funcs :test-input-txts-dirs-paths)
            (repeat "| efficiency: ")
            efficiencies))
        ]
    (is (= ((nth precisions-recalls 0) :recall)    1.0    ))
    (is (= ((nth precisions-recalls 1) :recall)    1.0    ))
    (is (= ((nth precisions-recalls 2) :recall)    1.0    ))
    (is (> ((nth precisions-recalls 3) :recall)    0.956  ))
    (is (> ((nth precisions-recalls 4) :recall)    0.958  ))
    (is (= ((nth precisions-recalls 5) :recall)    1.0  ))))
