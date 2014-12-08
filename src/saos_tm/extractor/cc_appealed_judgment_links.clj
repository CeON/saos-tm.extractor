(ns saos-tm.extractor.cc-appealed-judgment-links
  (:require
   [ clojure.string :as str ]
   [saos-tm.extractor.common :refer :all])
  (:import java.io.File)
  (:gen-class))

(def osp-signature-regex
  "[IVXLCDM]+[\\s\\.]+[0-9]*[\\s\\.]*[a-zA-Z]*-?[a-zA-Z]*[\\s\\.]+\\d+/\\d+")

(defn find-appeal [appeal-indicator s]
  (re-find
   (re-pattern
    (str appeal-indicator
         "((?!" osp-signature-regex ")[\\s\\S])*"
         osp-signature-regex))
   s))

(defn extract-signature [s]
  (let [
        appeal-match-groups
          (re-find
           (re-pattern
            (str "na skutek apelacji"
                 "((?!" osp-signature-regex ")[\\s\\S])*"
                 osp-signature-regex))
           s)
        appeal-match-str (first appeal-match-groups)
        without-newlines
          (when
            (not-nil? appeal-match-str)
            (str/replace
             appeal-match-str
             (re-pattern system-newline) ""))
        ]
    without-newlines
    ))

(defn extract-signatures-from-file [path]
  (let [
        content (slurp path)
        judgments nil
        first-instances (map #(extract-signature %) judgments)
        to-file (str/join system-newline first-instances)
        _ (spit "cc-appealed.txt" to-file)
        ]))
