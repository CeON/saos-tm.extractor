(ns saos-tm.extractor.main
  (:require
    [ saos-tm.extractor.builder :as builder ]
    [ saos-tm.extractor.extractor :as extractor ]
    )
  (:import java.io.File)
  (:gen-class))

(defn -main [ & argv ]
  (if (= (nth argv 0) "-extract")
    (extractor/extract-law-links-from-file
      (nth argv 1)
      (nth argv 2)
      (nth argv 3))
    (builder/process
      (nth argv 0)
      (nth argv 1)
      (nth argv 2))))
