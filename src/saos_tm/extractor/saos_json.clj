(ns saos-tm.extractor.saos-json
  (:require
   [clojure.string :as str]
   [cheshire.core :as cheshire]
   [saos-tm.extractor.common :as common]
   [saos-tm.extractor.csv-helpers :as csv-helpers])
  (:gen-class))

(defn ^:private get-nth-args [csv-string]
  (let [
        lines (str/split-lines csv-string)
        nth-args
          (map
           #(first
             (str/split % (re-pattern common/csv-delimiter)))
           lines)
        without-quots
          (map
           #(apply str (drop 1 (drop-last 1 %)))
           nth-args)
        ]
    without-quots))

(defn ^:private spit-contents [path ids contents]
  (doall
   (map
    #(spit (str path %1) %2) ids contents)))

(defn ^:private spit-judgments-from-json [json]
  (let [
        sentences (filter #(= (% "judgmentType") "SENTENCE") json)
        sentences (take 3 (shuffle sentences))
        ids (map #((% "source") "judgmentId") sentences)
        contents (map #(% "textContent") sentences)
        _ (spit-contents "test-data/cc-appealed/reg-sentence/" ids contents)
        ]
    ))

(defn ^:private get-jsons [path]
  (let [
        jsons-paths
          (common/get-file-paths path #"[\s\S]*\.json")
        _ (prn jsons-paths)
        jsons-strings (map #(slurp %) jsons-paths)
        jsons (map #(cheshire/parse-string %) jsons-strings)
        ]
    jsons))

(defn ^:private spit-judgments-by-ids [json ids]
  (let [
        sentences (filter #(some #{((% "source") "judgmentId")} ids) json)
        _ (prn (count sentences))
        ids (map #((% "source") "judgmentId") sentences)
        _ (prn (count ids))
        contents (map #(% "textContent") sentences)
        _ (spit-contents "test-data/osp-parties/answers-3/" ids contents)
        ]
    ))

(defn generate-appeal-test-set []
  (let [
        jsons (get-jsons "test-data/cc-appealed/reg-json/")
        _ (doall (map #(spit-judgments-from-json %) jsons))
        ]
    ))

(defn generate-osp-parties-test-set []
  (let [
        ids
          (csv-helpers/get-nth-args
           (slurp "test-data/osp-parties/answers-3.txt"))
        jsons (get-jsons "/home/floydian/icm/osp/json/4/")
        json (flatten jsons)
        _ (spit-judgments-by-ids json ids)
        ]
    ))


