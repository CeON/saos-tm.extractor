(ns saos-tm.extractor.main
  (:require
    [ saos-tm.extractor.law-links-trainset :as law-links-trainset ]
    [ saos-tm.extractor.law-links :as law-links ])
  (:import java.io.File)
  (:gen-class))

(defn -main [ & argv ]
  (if (= (nth argv 0) "-extract")
    (law-links/extract-law-links-from-file
      (nth argv 1)
      (nth argv 2)
      (nth argv 3))
    (law-links-trainset/process
      (nth argv 0)
      (nth argv 1)
      (nth argv 2))))
