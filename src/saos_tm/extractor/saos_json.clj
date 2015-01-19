(ns saos-tm.extractor.saos-json
  (:require
   [clojure.string :as str]
   [cheshire.core :refer :all]
   [saos-tm.extractor.common :refer :all]
   [saos-tm.extractor.csv-helpers :refer :all]
   [saos-tm.extractor.common-test :refer :all])
  (:gen-class))

(defn spit-contents [path ids contents]
  (doall
   (map
    #(spit (str path %1) %2) ids contents)))

(defn spit-judgments-from-json [json]
  (let [
        sentences (filter #(= (% "judgmentType") "SENTENCE") json)
        sentences (take 3 (shuffle sentences))
        ids (map #((% "source") "judgmentId") sentences)
        contents (map #(% "textContent") sentences)
        _ (spit-contents "test-data/cc-appealed/reg-sentence/" ids contents)
        ]
    ))

(defn get-jsons [path]
  (let [
        jsons-paths
          (get-file-paths path #"[\s\S]*\.json")
        _ (prn jsons-paths)
        jsons-strings (map #(slurp %) jsons-paths)
        jsons (map #(parse-string %) jsons-strings)
        ]
    jsons))

(defn generate-appeal-test-set []
  (let [
        jsons (get-jsons "test-data/cc-appealed/reg-json/")
        _ (doall (map #(spit-judgments-from-json %) jsons))
        ]
    ))

(defn spit-judgments-by-ids [json ids]
  (let [
        sentences (filter #(some #{((% "source") "judgmentId")} ids) json)
        _ (prn (count sentences))
        ids (map #((% "source") "judgmentId") sentences)
        _ (prn (count ids))
        contents (map #(% "textContent") sentences)
        _ (spit-contents "test-data/osp-parties/answers-3/" ids contents)
        ]
    ))

(defn generate-osp-parties-test-set []
  (let [
        ids (get-nth-args (slurp "test-data/osp-parties/answers-3.txt"))
        jsons (get-jsons "/home/floydian/icm/osp/json/4/")
        json (flatten jsons)
        _ (spit-judgments-by-ids json ids)
;;         _ (spit "ids.txt" (apply str (map #(apply str % system-newline) ids)))
        ]
    ))
