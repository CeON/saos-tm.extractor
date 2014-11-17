(ns saos-tm.extractor.osp-extract-parties
  (:require
    [ clojure.string :as str ]
    [ clojure.xml :as xml ]
    [ clojure.zip :as zip ]
    [saos-tm.extractor.common :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn extract-osp-judgments [s]
  (map
    #(first %)
    (re-seq #"\<judgement((?!\<judgement)[\s\S])*\</judgement\>" s)))

(defn extract-osp-judgment-id [s]
  (re-find #"(?<=id=').\d+[_a-zA-Z0-9\-]+" s))

; removing newlines if are inserted in content
; clj.xml/emit inserts newlines in content of xml nodes
(defn convert-emit [s]
  (replace-several s
    (re-pattern (str "(?<=\\>)" system-newline "(?=[^\\<])")) ""
    (re-pattern (str "(?<=[^\\>])" system-newline "(?=\\<)")) ""))

(defn emit-xml [xml-tree]
  (convert-emit
    (with-out-str
      (xml/emit xml-tree))))

(defn emit-xmls [to-remove-1 to-remove-2]
[(emit-xml
  (->
    to-remove-1
    zip/remove
    zip/root))
(emit-xml
  (->
    to-remove-2
    zip/remove
    zip/next
    zip/remove
    zip/root))])

(defn split-osp-judgment-to-parts [s]
  (let [
          root (zip-str s)
          to-remove-1
            (->
              root
              zip/next
              zip/next
              zip/right
              zip/next
              zip/right
              zip/right)
          to-remove-2
            (->
              root
              zip/next
              zip/next
              zip/right
              zip/next)
    ]
    (if
      (or
        (nil? to-remove-1)
        (nil? to-remove-2))
      [(emit-xml (zip/node root))]
      (emit-xmls to-remove-1 to-remove-2))))

(defn cleanse-party [s]
  (when
    (not-nil? s)
      (str/trim
        (replace-several s
           #"\-\</xText\>" "</xText>"
           (re-pattern system-newline) ""
           #"\s+" " "
           #"z odwołania" ""
           #"z powództwa" ""
           #"powództwa" ""
           #"z wniosku" ""
           #"wnioskodawc(zyń:?|zyni|y|ów:?)" ""
           #"w wniosku" ""
           #"stronie pozwanej" ""
           #"pozwan(ej|emu|ych|ego)" ""
           #"oskarżon(ej|emu|ych|ego)" ""
           #"skazan(ej|emu|ych|ego)" ""
           #"^ie" ""
           #"^y" ""
           #"^u\s" ""
           #"^\s*:" ""
           #"^\d+" ""
           #"­\." ""
           #"-$" ""
           #"\>\d+" ">"
          ;#"\.\<" ""
          ;#"^\s*\((\.)*\)" ""
          ))))

(defn identify-plaintiff [s]
  (first
    (str/split
      s
      (re-pattern
        (str
          "przeciwko")))))

(defn close-opening-tag [s]
  (if
    (nil?
      (re-find #"^\<xText\>" s))
    (str "<xText>" s)
    s))

(defn close-closing-tag [s]
  (if
    (nil?
      (re-find #"\</xText\>$" s))
    (str s "</xText>")
    s))

(defn close-xtext-tags [s]
  (let [
          opening-tag-closed (close-opening-tag s)
          both-tags-closed (close-closing-tag opening-tag-closed)
    ]
    both-tags-closed))

(defn extract-plaintiff [s]
  (close-xtext-tags
    (cleanse-party
      (identify-plaintiff
        (first
          (str/split s #"\</xText\>"))))))

(defn get-regex-match [regex to-next-xtext s]
  (re-find
    (re-pattern
      (str regex to-next-xtext))
    s))

(defn get-first-regex-match [coll to-next-xtext s]
  (let [
          matches
            (map
              #(get-regex-match % to-next-xtext s)
              coll)
    ]
    (find-first
      #(not-nil? %)
      matches)))

(defn get-first-regex-match-case-ins [coll to-next-xtext s]
  (get-first-regex-match
    (map #(str "(?i)" %) coll)
    to-next-xtext
    s))

(defn identify-defendant [s]
  (first
    (str/split
      s
      (re-pattern
        (str
           "w przedmiocie|"
           "\\so\\s|"
           "-o\\s|"
           "oskarżonego z|"
           "osk\\. z|"
           ;"skazane(j|go)|"
           "o zasądzenie|"
           "o odszkodowanie|"
           "odszkodowanie")))))

(defn remove-xLexLink-tags [s]
  (str/replace s
    #"\<xLexLink((?!\<xLexLink)[\s\S])*\</xLexLink\>" ""))

(defn handle-defendant [s]
  (close-xtext-tags
    (cleanse-party
      (identify-defendant s))))

(defn extract-defendant [s]
  (let [
        to-next-xtext "((?!\\</xText\\>)[\\s\\S])*(?=\\</xText\\>)"
        match
          (get-first-regex-match
            ["(?<=sprawy z wniosku?\\</xText\\>)"
             "(?<=sprawy z wniosku?)"
             "(?<=sprawy z wniosku)"
             "(?<=przeciwko\\</xText\\>)"
             "(?<=przeciwko)"
             "(?<=z wniosku)"
             "(?<=sprawy?\\</xText\\>)"
             "(?<=s p r a w y)"
             "(?<=sprawy)"]
             to-next-xtext
             s)
        ]
        (when
          (not-nil? match)
          (if
            (string? match)
            (handle-defendant match)
            (handle-defendant (first match))))))

(defn extract-parties-osp [s]
  (let [
          whatever "[\\s\\S]*"
          without-lex-links (remove-xLexLink-tags s)
          match
            (get-first-regex-match-case-ins
              ["(?<=przy udziale)"
               "(?<=w obecności)"
               "prokurator prokuratury"
               "prokuratora prok"
               "(?<=sprawy z wniosku)"
               "(?<=spraw z wniosków)"
               "(?<=sprawy z powództwa)"
               "(?<=spraw z powództw)"
               "(?<=sprawy z odwołania)"
               "(?<=spraw z odwołań)"
               "(?<=z powództwa)"
               "(?<=z powództw)"
               "(?<=z odwołania)"
               "(?<=z odwołań)"
               "(?<=z wniosku)"
               "(?<=z wniosków)"
               "(?<=w sprawie ze skargi)"
               "(?<=po rozpoznaniu w sprawie)"
               "(?<=w sprawie)"
               "(?<=w obecności oskarżyciela publ)"
               "(?<=<xText>sprawy)"]
               whatever
               without-lex-links)
          id (extract-osp-judgment-id without-lex-links)
    ]
    (if
      (nil?
        (re-find
          (re-pattern
            (str
              "(?i)\\<xName\\>\\s*postanowienie.?\\s*\\</xName\\>|"
              "(?i)\\<xName\\>\\s*uzasadnienie.?\\s*\\</xName\\>"))
         without-lex-links))
      (if
        (nil? match)
        id
        (zipmap
          [:plaintiff :defendant
           ;:txt
           :id]
          [(extract-plaintiff match) (extract-defendant match)
           ;match
           id]))
      (prn id))))

(defn dexmlise-cleanse [s]
  (when (not-nil? s)
    (str/trim (dexmlise s))))

(defn dexmlise-parties-osp [coll]
  (map
    #(if (map? %)
      (zipmap
      [:plaintiff :defendant
       ;:txt
       :id]
      [(dexmlise-cleanse (:plaintiff %))
       (dexmlise-cleanse (:defendant %))
       ;(:txt %)
       (:id %)])
      %)
    coll))

(defn join-newline [coll1 coll2 coll3]
  (let [
          pairs
            (map
              #(str %1
                system-newline "\t" %2
                ;system-newline %3
                )
              coll1
              coll2
              ;coll3
              )
    ]
    (clojure.string/join
      system-newline
      pairs)))

(defn remove-xtexts [s]
  (str/replace s #"\<xText/\>" ""))

(defn extract-parties-from-judgments [coll]
  (remove nil?
          (map
           #(extract-parties-osp
             (first
              (split-osp-judgment-to-parts %)))
           coll)))

(defn extract-parties-from-txt [s]
  (extract-parties-from-judgments
    (extract-osp-judgments s)))

(defn count-osp-judgments [s]
  (count
    (extract-osp-judgments s)))

(defn get-file-paths [dir re]
  (let [
          file-paths
            (.listFiles
              (clojure.java.io/file dir))
          file-paths
            (take 10
              (filter #(matches? (str %) re)
                file-paths))
        ]
    file-paths))

(defn take-to-regex [s re]
  (first
   (str/split s re)))

(defn contains-some [s coll]
  (some
   #(substring? % s)
   coll))

; removed <xText/> tags
(defn get-judgments [file-paths]
  (let [
          files
            (map
              #(slurp %)
              file-paths)
          ss
            (map
              #(remove-xtexts %)
              files)
          judgments (map #(extract-osp-judgments %) ss)
        ]
    judgments))

(def test-set-xml-path "test-data/osp-parties/test-set.xml")

(defn extract-osp-test-xml []
  (let [
          ids-file (slurp "test-data/osp-parties/answers.txt")
          ids-lines
            (str/split ids-file (re-pattern system-newline))
          ids
            (map
             #(take-to-regex %
               (re-pattern
                (str "\"" csv-delimiter "\"")))
             ids-lines)
          ids
            (map
             #(str/replace % #"\"" "")
             ids)
          file-paths
            (get-file-paths
             "/home/floydian/saos-ext/test-data/osp-parties-copy"
             #"[\s\S]*_con\.xml")
          judgments (apply concat (get-judgments file-paths))
          judgments
            (filter
             #(contains-some % ids)
             judgments)
          judgments-str
            (clojure.string/join
              system-newline
              judgments)
          opening-str
            (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                 system-newline
                 "<judgements>")
          closing-str "</judgements>"
          xml
            (str opening-str
                 system-newline
                 judgments-str
                 system-newline
                 closing-str)
          _ (spit test-set-xml-path xml)
        ]))

(defn spit-parties []
  (let [
          file-paths
            (get-file-paths
             "/home/floydian/saos-ext/test-data/osp-parties-copy"
             #"[\s\S]*_con\.xml")
          file-paths [test-set-xml-path]
          judgments (get-judgments file-paths)
          osp-parties
            (mapcat
              #((dexmlise-parties-osp
                 (extract-parties-from-judgments %))
                judgments))
              ;#(dexmlise-parties-osp (extract-parties-from-txt %)) ss)
          ids-not-extracted
            (filter
              #(string? %)
              osp-parties)
          osp-parties
            (remove
              #(string? %)
              osp-parties)
          defendants
            (map
              #(:defendant %)
              osp-parties)
          plaintiffs
            (map
              #(:plaintiff %)
              osp-parties)
          ids
            (map
              #(:id %)
              osp-parties)
          txts
            (map
              #(:txt %)
              osp-parties)
    ]
    (spit "tmp/defendants.txt"
      (join-newline defendants ids txts))
    (spit "tmp/plaintiffs.txt"
      (join-newline plaintiffs ids txts))))

(defn get-osp-judgment-by-id [id s]
  (apply str
    (filter
      #(not-nil?
        (re-find (re-pattern id) %))
      (extract-osp-judgments s))))
