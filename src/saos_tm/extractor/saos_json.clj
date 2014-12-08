(ns saos-tm.extractor.saos-json
  (:require
   [ clojure.string :as str ]
   [cheshire.core :refer :all]
   [saos-tm.extractor.common-test :refer :all])
  (:gen-class))

(defn spit-judgments-from-json [json]
  (let [
        sentences (filter #(= (% "judgmentType") "SENTENCE") json)
        sentences (take 3 (shuffle sentences))
        ids (map #((% "source") "judgmentId") sentences)
        contents (map #(% "textContent") sentences)
        _
          (doall
            (map
             #(spit (str "test-data/cc-appealed/reg-sentence/" %1) %2)
             ids
             contents))
        ]
    ))

(defn generate-appeal-test-set []
  (let [
        jsons-paths
          (get-file-paths
             "test-data/cc-appealed/reg-json/"
             #"[\s\S]*\.json")
        _ (prn jsons-paths)
        jsons-strings (map #(slurp %) jsons-paths)
        jsons (map #(parse-string %) jsons-strings)
        _ (doall (map #(spit-judgments-from-json %) jsons))
        ]
    ))
