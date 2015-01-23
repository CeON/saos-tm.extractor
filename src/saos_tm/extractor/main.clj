(ns saos-tm.extractor.main
  (:require
    [clojure.string :as str]
    [saos-tm.extractor.common :refer
      [csv-delimiter system-newline  get-csv-for-extracted-link get-art-coords-csv ]]
    [saos-tm.extractor.law-links-trainset :as law-links-trainset]
    [saos-tm.extractor.law-links :as law-links]
    [saos-tm.extractor.judgment-links :as judgment-links])
  (:import java.io.File)
  (:gen-class))

(defn get-csv-for-orphaned-link [link signature]
  (let [
        art (:art link)
        txt (:txt link)
        ]
    (apply str
           "\"" txt "\"" csv-delimiter
           (apply str
                  (map
                    #(str "\"" % "\"" csv-delimiter)
                    art))
           "\"" signature "\"" system-newline)))

(defn get-csv-for-links [get-csv-func links signature]
  (str/join ""
            (map
              #(get-csv-func % signature)
              links)))

(defn extract-law-links-from-file
  [input-file-path output-file-path orphaned-links-file-path
   dictionary-file-path signature]
  (time
    (let [
          dictionary (law-links/load-dictionary dictionary-file-path)
          input-txt (slurp input-file-path)
          signature
          (if (nil? signature)
            (law-links/extract-signature (law-links/get-line-with-signature input-txt))
            signature)
          signature-file-name
          (last
            (str/split input-file-path #"/"))
          links
          (law-links/extract-law-links input-txt dictionary)
          ]
      (spit
        output-file-path
        (get-csv-for-links
          get-csv-for-extracted-link
          (:extracted-links links)
          signature))
      (spit orphaned-links-file-path
            (get-csv-for-links
              get-csv-for-orphaned-link
              (:orphaned-links links)
              signature)))))

(defn -main [ & argv ]
  (if (= (nth argv 0) "-extract")
    (if (and (> (count argv) 5) (= (nth argv 5) "-signature"))
      (extract-law-links-from-file
        (nth argv 1)
        (nth argv 2)
        (nth argv 3)
        (nth argv 4)
        (nth argv 6))
      (extract-law-links-from-file
        (nth argv 1)
        (nth argv 2)
        (nth argv 3)
        (nth argv 4)
        nil))
    (if (= (nth argv 0) "-extract-judgment")
      (judgment-links/extract-signatures-from-file
        (nth argv 1)
        (nth argv 2))
      (law-links-trainset/process
        (nth argv 0)
        (nth argv 1)
        (nth argv 2)))))
