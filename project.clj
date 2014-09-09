(defproject saos-tm.extractor "0.2.0-SNAPSHOT"
  :description "SAOS metadata extractor module."
  :url "https://github.com/CeON/saos-tm.extractor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot :all
  :main saos-tm.extractor.main

  :dependencies [
    [org.clojure/clojure "1.6.0"]
    [clojure-opennlp "0.3.2"]
    [langlab "1.1.0"] 
    [com.taoensso/timbre "3.2.1"]]

   :plugins [
     [lein-ancient "0.5.5"]])
