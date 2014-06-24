(ns saos-tm.extractor.core
  (:require
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    )
  (:import java.io.File)
  (:gen-class))

(def csv-delimiter ",")

(defn substring? [sub st]
  (not= (str/.indexOf st sub) -1))

(defn extract-nmbs-and-ranges [ s ]
  (map #(str/trim %)
    (str/split s #",|i|(oraz)")))

(defn get-coords-names [is-art is-par is-ust is-pkt is-zd is-lit]
  (filter #(not= "" %)
    [(if is-art "art" "")
     (if is-par "par" "")
     (if is-ust "ust" "")
     (if is-pkt "pkt" "")
     (if is-zd "zd." "")
     (if is-lit "lit." "")]))

(defn convert-ranges-to-single-records [record]
  [(flatten (record "art"))
  (flatten (record "par"))
  (flatten (record "ust"))
  (flatten (record "pkt"))
  (flatten (record "zd."))
  (flatten (record "lit."))])

(defn change-empty [value]
  (if (empty? value)
    ["0"]
    value))

(defn convert-empties [art-coords]
  (map #(change-empty %) art-coords))

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
      more (cartesian-product (rest colls))]
      (cons x more))))

(defn remove-trailing-cojunction [s]
  (let [
          tokens (lg-split-tokens-bi "pl" s)
          last-token (last tokens)
          ]
  (if (or (= last-token "i") (= last-token "z") (= last-token "oraz"))
    (str/trim
      (apply str
        (drop-last (count last-token) s)))
    (str/trim s))))

(defn extract-coords-for-single-art [s]
  (cartesian-product
    (convert-empties
      (convert-ranges-to-single-records
        (zipmap
          (get-coords-names
            (substring? "art." s)
            (substring? "§" s)
            (substring? "ust." s)
            (substring? "pkt" s)
            (substring? "zd." s)
            (substring? "lit." s))
          (map extract-nmbs-and-ranges
            (drop 1
              (str/split s #"art\.|§|ust\.|pkt|zd\.|lit\."))))))))

(defn extract-coords [s]
  (let [
          trimmed (str/trim s)
          separate-art-coords
            (if (substring? "art." trimmed)
              (map
                #(apply str "art." %)
                (drop 1 (str/split trimmed #"art\.")))
              [trimmed])
          separate-art-coords-trimmed
            (map
              #(str/trim %)
              separate-art-coords)
          without-conjunctions
            (map
              #(remove-trailing-cojunction %)
              separate-art-coords-trimmed)
          ]
  (mapcat extract-coords-for-single-art without-conjunctions)))


