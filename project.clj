(defproject saos-tm.extractor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot :all
  :main saos-tm.extractor.main

  :dependencies [
    [org.clojure/clojure "1.6.0"]
    [clojure-opennlp "0.3.2"]
    [langlab "1.1.0"] ]

   :plugins [
     [lein-ancient "0.5.5"]])
