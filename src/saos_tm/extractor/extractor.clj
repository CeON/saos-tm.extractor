(ns saos-tm.extractor.extractor
  (:require
    [ saos-tm.extractor.core :as core ]
    [ clojure.string :as str ]
    [ langlab.core.parsers :refer [ lg-split-tokens-bi ] ]
    )
  (:import java.io.File)
  (:gen-class))

(defn split-tokens [s]
  (lg-split-tokens-bi "pl" s))

(defn find-first [f coll]
  (first (filter f coll)))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn in? [seq elm]
  (some #(= elm %) seq))

(def coords-tokens
  ["." "," "Art" "art" "ust" "par" "§" "pkt" "zd" "i" "oraz" "lub" "z" "-" "a" "także"])

(defn not-coords-nmb? [s]
  (let [
          result (re-matches #"(\d+)(-\d+)?[a-z]*" s)
          ]
    (if (= result nil) true false))) 

(defn not-coord-token? [token]
  (let [
          result (in? coords-tokens token)
          ]
  (if (= result nil) true false)))

(defn get-coords-tokens [first-token-index tokens]
  (first
    (indices
      #(and (not-coord-token? %) (not-coords-nmb? %))
      (drop first-token-index tokens))))

(defn find-coords-ranges [first-token-index tokens]
  (let [
          first-non-coord-token-index
            (get-coords-tokens first-token-index tokens)
        ]
  [first-token-index
  (if (nil? first-non-coord-token-index)
    (inc (count tokens))
    (+ first-token-index first-non-coord-token-index))]))

(defn get-range [coll from to]
  (take (- to from) (drop from coll)))

(defn get-range* [coll fromto]
  (get-range coll (first fromto) (second fromto)))

(defn w-zwiazku-z? [tokens]
  (true?
    (and
      (or (= 3 (count tokens)) (= 4 (count tokens)))
      (= "w" (first tokens))
      (or (= "związku" (second tokens)) (= "zw" (second tokens))))))

(defn handle-w-zwiazku-z [tokens-and-coords]
  (for [
          i (range 0 (count tokens-and-coords))
          ]
    (if (w-zwiazku-z? (nth tokens-and-coords i))
      (nth tokens-and-coords (+ i 1))
      (nth tokens-and-coords i))))

(def dictionary-for-acts '{"Konstytucji" {:nr "78" :poz "483"}
                           "k.c" {:nr "16" :poz "93"}
                           "k.h" {:nr "57" :poz "502"}
                           "k.k" {:nr "88" :poz "553"}
                           "k.k.s" {:nr "83" :poz "930"}
                           "k.k.w" {:nr "90" :poz "557"}
                           "k.m" {:nr "138" :poz "1545"}
                           "k.p" {:nr "24" :poz "141"}
                           "k.p.a" {:nr "30" :poz "168"}
                           "k.p.c" {:nr "43" :poz "296"}
                           "k.p.k" {:nr "89" :poz "555"}
                           "k.p.w" {:nr "106" :poz "1148"}
                           "k.r.o" {:nr "9" :poz "59"}
                           "k.s.h" {:nr "94" :poz "1037"}
                           "k.w" {:nr "12" :poz "114"}
                           "k.z" {:nr "82" :poz "598"}
                           })

(defn extract-law-act [tokens]
  (let [
          dictionary-record (dictionary-for-acts (first tokens))
          ]
  (if (nil? dictionary-record)
    tokens
    dictionary-record)))

(defn extract-nr-poz [tokens]
  (let [
          nr-indices (indices #(= % "Nr") tokens)
          nr-index
            (if (nil? nr-indices)
              nil
              (first nr-indices))
          poz-indices (indices #(= % "poz") tokens)
          poz-index
            (if (nil? poz-indices)
              nil
              (first poz-indices))
          ]
  (if (or (nil? nr-index) (nil? poz-index))
    (extract-law-act tokens)
    (zipmap
      [:nr :poz]
      [(if (nil? nr-index) "0" (nth tokens (+ nr-index 1)))
      (if (nil? poz-index) "0" (nth tokens (+ poz-index 2)))]))))

(defn coord-to-text [token]
  (if (or (= "." token) (= "-" token))
    token
    (str " " token)))

(defn build-coords-text [tokens-range tokens]
  (str/replace
    (str/join ""
      (map
        coord-to-text
        (get-range* tokens tokens-range)))
    "- " "-"))

(defn get-interfering-coords-ranges [tokens]
  (map #(find-coords-ranges % tokens) 
    (indices 
      #(or (= % "art") (= % "Art") (= % "§")) 
      tokens)))

(defn get-inter-coords-ranges [tokens]
  (let [
          interfering-coords-ranges (get-interfering-coords-ranges tokens)
          ]
  (filter
    #(< (first %) (second %))
    (partition 2
      (concat
        (drop 1
          (flatten interfering-coords-ranges))
        [(count tokens)])))))

(defn get-correct-coords-ranges [tokens]
  (let [
          interfering-coords-ranges (get-interfering-coords-ranges tokens)
          inter-coords-ranges (get-inter-coords-ranges tokens)
          ]
  (partition 2
    (concat
    [(first (first interfering-coords-ranges))]
    (flatten inter-coords-ranges)))))

(defn get-majority-act-coords-for-art-coords [art-coord art-act-coords]
  (let [
        sorted
          (sort-by val >
            (frequencies
              (map
                #(:act %)
                (filter
                  #(= art-coord (:art %))
                  art-act-coords))))
          ]
  [art-coord 
  (first
    (find-first
      #(map? (first %))
      sorted))]))

(defn change-act-coords-to-majorities [majority-vote-act-coord art-act-coords]
  (let [
          art-coord (first majority-vote-act-coord)
          act-coord (second majority-vote-act-coord)
          records-for-art-coord
            (filter
              #(= art-coord (:art %))
              art-act-coords)]
  (if (nil? act-coord)
    records-for-art-coord
    (zipmap [:art :act] [art-coord act-coord]))))

(defn extract-signature [s]
  (-> (str/replace s "Sygn." "")
    (str/replace "akt" "")
    (str/replace "(" "")
    (str/replace ")" "")
    (str/replace "*" "")
    (str/trim)))

(defn get-line-with-signature [s]
  (let [
          lines (str/split s #"\n")
          lines-with-sygn-text (filter #(.startsWith % "Sygn.") lines)
          index-of-first-line-ending-with-date
            (first
              (indices
                #(.endsWith % " r.")
                lines))
    ]
  (if (empty? lines-with-sygn-text)
    (nth lines (+ index-of-first-line-ending-with-date 1))
    (first lines-with-sygn-text))))

(def art-coords-names [:art :par :ust :pkt :zd :lit])

(defn create-map-for-art-coords [art-coords]
  (zipmap art-coords-names art-coords))

(defn get-data-for-act-art [art-act]
  (let [
          art (:art art-act)
          act (:act art-act)
    ]
    (map #(zipmap [:art :act] [(create-map-for-art-coords %1) act]) art)))

(defn get-art-coords-csv [art-coords]
  (let [
          art-nr (:art art-coords)
          par-nr (:par art-coords)
          ust-nr (:ust art-coords)
          pkt-nr (:pkt art-coords)
          zd-nr (:zd art-coords)
          lit-nr (:lit art-coords)
    ]
    (apply str
      "\"" art-nr "\"" core/csv-delimiter
      "\"" par-nr "\"" core/csv-delimiter
      "\"" ust-nr "\"" core/csv-delimiter
      "\"" pkt-nr "\"" core/csv-delimiter
      "\"" zd-nr "\"" core/csv-delimiter
      "\"" lit-nr "\"" core/csv-delimiter)))

(defn get-csv-for-extracted-link [link signature]
  (let [
          art (:art link)
          act (:act link)
    ]
  (apply str (get-art-coords-csv art)
    "\"" (:nr act) "\"" core/csv-delimiter
    "\"" (:poz act) "\"" core/csv-delimiter
    "\"" signature "\"" "\n")))

(defn get-csv-for-orphaned-link [link signature]
  (let [
          art (:art link)
          txt (:txt link)
    ]
    ; (println link)
    (apply str
      "\"" txt "\"" core/csv-delimiter
      (apply str
        (map
          #(str "\"" % "\"" core/csv-delimiter)
          art))
      "\"" signature "\"" "\n")))

(defn get-csv-for-links [get-csv-func art-act-coords signature]
  (str/join ""
    (map
      #(get-csv-func % signature)
      art-act-coords)))

(defn get-data-for-orphaned-link [orphaned-link]
  (let [
          txt (str/join " " (take 10 (:act orphaned-link)))
    ]
  (map #(zipmap [:txt :art]
                [(str/replace
                  (str/replace
                    txt
                    " ." ".")
                  " ," ",") %])
                 (:art orphaned-link))))
  
(defn extract-law-links [s]
  (let [
        tokens (lg-split-tokens-bi "pl" s)
        interfering-coords-ranges (get-interfering-coords-ranges tokens)
        inter-coords-ranges (get-inter-coords-ranges tokens)
        correct-coords-ranges (get-correct-coords-ranges tokens)
        coords-texts
          (map
            #(build-coords-text % tokens)
            correct-coords-ranges)
        coords
          (map 
            core/extract-coords
            (map #(build-coords-text % tokens) correct-coords-ranges))
        act-coords-part 
          (handle-w-zwiazku-z
            (map extract-nr-poz
              (map 
                #(get-range tokens (first %) (second %)) 
                inter-coords-ranges)))
        art-act-coords-part
          (map #(zipmap [:art :act] [%1 %2])
            coords act-coords-part)
        distinct-art-coords-part
          (distinct
            (map #(:art %)
              art-act-coords-part))
        majority-votes-for-act-coords
          (map
            #(get-majority-act-coords-for-art-coords % art-act-coords-part)
            distinct-art-coords-part)
        art-act-coords-after-majority-voting
          (map
            #(change-act-coords-to-majorities % art-act-coords-part)
            majority-votes-for-act-coords)
        extracted-links
          (filter
            #(map? (:act %))
            art-act-coords-after-majority-voting)
        orphaned-links
          (flatten
            (remove
              #(map? (:act %))
              art-act-coords-after-majority-voting))
        ]
  (->>
    (zipmap
    [:extracted-links :orphaned-links]
    [(mapcat get-data-for-act-art extracted-links)
     (mapcat get-data-for-orphaned-link orphaned-links)]))))

(defn extract-law-links-from-file
  [input-file-path output-file-path orphaned-links-file-path]
  (let [
          input-txt (slurp input-file-path)
          signature (extract-signature (get-line-with-signature input-txt))
          signature-file-name (last (str/split input-file-path #"/"))
          links (extract-law-links input-txt)
        ]
  ; links))
  (spit output-file-path
    (get-csv-for-links
      get-csv-for-extracted-link
      (:extracted-links links)
      signature))
  (spit orphaned-links-file-path
    (get-csv-for-links
      get-csv-for-orphaned-link
      (:orphaned-links links)
      signature))))
