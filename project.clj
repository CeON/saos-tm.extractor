(defproject saos-tm.extractor "1.0.0"
  :description "SAOS metadata extractor module."
  :url "https://github.com/CeON/saos-tm.extractor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot :all

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.apache.tika/tika-parsers "1.6"]
                 [commons-io/commons-io "2.4"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [cheshire "5.3.1"]
                 [langlab "1.2.0"]
                 [automat "0.1.3"]]

   :plugins [ [codox "0.8.12"] ]

   :codox {
           :output-dir "target/apidoc"
           :sources [ "src"]
           :defaults {:doc/format :markdown}
           :src-dir-uri "https://github.com/CeON/saos-tm.extractor/blob/master/"
           :src-linenum-anchor-prefix "L"
          }
)
