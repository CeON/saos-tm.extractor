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

(defn emit-xmls-2 [to-remove]
[(emit-xml
  (->
    to-remove
    zip/right
    zip/remove
    zip/root))
 (emit-xml
  (->
    to-remove
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
      (if (= :xText (:tag (first to-remove-2)))
         [(emit-xml (zip/node root))]
         (emit-xmls-2 to-remove-2))
      (emit-xmls to-remove-1 to-remove-2))))

(defn xml-tag-regex [tag-name]
  (re-pattern
   (str "\\<" tag-name "((?!\\>)[\\s\\S])*\\>")))

(defn cleanse-party [s]
  (when
    (not-nil? s)
    (str/trim
     (replace-several s
                      ;#"\>,\<" "><"
                      ;#"\>,\<((?!\\>)[\\s\\S])*\\>$" ""

                      (xml-tag-regex "xText") ""
                      #"\</xText\>" ""
                      #"\<xText\>" ""
                      #"\<xText" ""
                      #"[^-]\-\</xText\>" "</xText>"
                      #"[^-]-$" ""
                      #"\.\</xText\>" "</xText>"

                      #"\<xBx\>" ""
                      #"\</xBx\>" ""
                      #"\<xBx/\>" ""
                      #"\<xBRx\>" ""
                      #"\</xBRx\>" ""
                      #"\<xBRx/\>" ""
                      #"\<xIx\>" ""
                      #"\</xIx\>" ""
                      #"\<xIx/\>" ""

                      (re-pattern system-newline) ""
                      #"\s+" " "
                      #"\>\s\<" "><"

                      #"(?<=[^A-Z])\.\s*$" ""
                      #";\s*$" ""
                      #",\s*$" ""
                      #"'" "\""

                      #"z odwołania" ""
                      #"z powództwa" ""
                      #"powództwa" ""
                      #"z wniosku" ""
                      #"wnioskodawc(zyń:?|zyni|y|ów:?)" ""
                      #"w wniosku" ""
                      #"stronie pozwanej" ""
                      #"pozwan(ej|emu|ych|ego)" ""

                      #"oskarżon(ej|emu|ych|ego) o czyny? z" ""
                      #"oskarżon(ej|emu|ych|ego) z" ""
                      #"oskarżon(ej|emu|ych|ego)" ""

                      #"skazan(ej|emu|ych|ego)" ""
                      #"obwinione(j|go)" ""
                      #"skarga" ""
                      #"^ie" ""
                      #"^y" ""
                      #"^u\s" ""
                      #"^\s*:" ""
                      #"­\." ""
                      ;#"^\d+" ""
                      ;#"\>\d+" ">"
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

(defn close-xText-tags [s]
  (let [
          opening-tag-closed (close-opening-tag s)
          both-tags-closed (close-closing-tag opening-tag-closed)
    ]
    both-tags-closed))

(defn get-first-split [s splitting-regexes]
   (let [
          splits
            (map
              #(str/split s %)
              splitting-regexes)
          splits
           (sort-by
            #(count (second %))
            splits)
    ]
    (last splits)))

(def not-matches? (complement matches?))

(defn close-xUnit-tag [s]
  (if
    (and
     (.startsWith s "<xUnit")
     (not-matches? s #"[\S\s]*</xUnit>[\S\s]*"))
    (str s "</xUnit>")
    s))

(defn extract-multiple [point-indicator defendant-match splitter s]
  (let [
        points-indicator-strs (str/split point-indicator #"1")
        to-first-point
          (first
           (str/split
            defendant-match
            (re-pattern
             (str "\\" (first points-indicator-strs)
                  "1" "\\" (second points-indicator-strs)))))
        points-indicator-str
          (str "\\" (first points-indicator-strs)
               "\\d"
               "\\" (second points-indicator-strs))
        points-indicator-regex (re-pattern points-indicator-str)
        points-indicators (re-seq points-indicator-regex s)
        points-indicators
          (partition-by #(substring? "1" %) points-indicators)
        points-indicators (flatten (take 2 points-indicators))
        defendants
          (map
           #(first
             (re-find
              (re-pattern
               (str "\\" (apply str (drop-last %))
                    "\\" (last %)
                    "((?!" splitter ")[\\s\\S])*(?=" splitter ")"))
              s))
           points-indicators)
        defendants-complete
          (sort
           (map #(str to-first-point %) defendants))
        defendants-complete
          (map
           #(close-xUnit-tag (cleanse-party %))
           defendants-complete)
        defendants-in-one-str (apply str defendants-complete)
        ]
    (close-xText-tags defendants-in-one-str)))

(defn extract-plaintiff [s]
  (let [
        plaintiff-match
          (first
           (str/split s #"po rozpoznaniu|przy udziale|\</xText\>"))
        point-indicator (re-find #"\>1[\.\<]" plaintiff-match)
        ]
    (if (nil? point-indicator)
      (close-xText-tags
        (cleanse-party plaintiff-match))
      (extract-multiple point-indicator plaintiff-match "</xText>" s))))

(defn get-regex-match [regex to-next-xText s]
  (re-find
    (re-pattern
      (str regex to-next-xText))
    s))

(defn get-first-regex-match [coll to-next-xText s]
  (let [
          matches
            (map
              #(get-regex-match % to-next-xText s)
              coll)
          match
            (find-first
             #(not-nil? %)
             matches)
          match
            (if (string? match)
              match
              (first match))
    ]
    match))

(defn get-first-regex-match-case-ins [coll to-next-xText s]
  (get-first-regex-match
    (map #(str "(?i)" %) coll)
    to-next-xText
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
           "o zasądzenie|"
           "o odszkodowanie|"
           "odszkodowanie|"
           "zapłat|"
           "prowadzącym działalność"
         )))))

(defn remove-xLexLink-tags [s]
  (str/replace s
    #"\<xLexLink((?!\<xLexLink)[\s\S])*\</xLexLink\>" ""))

(defn handle-defendant [s]
  (close-xText-tags
    (cleanse-party s)))

(def to-next-xText "((?!\\</xText\\>)[\\s\\S])*(?=\\</xText\\>)")

(defn get-first-defendant-end-indicator-match
  [defendant-indicators defendant-end-indicators s]
  (let [
          matches
            (map
              #(get-first-regex-match defendant-indicators % s)
              defendant-end-indicators)
          match
            (find-first
             #(not-nil? %)
             matches)
          match
            (if (string? match)
              match
              (first match))
    ]
    match))

(defn print-if-contains [s element]
  (if (substring? element s) (prn s)))

(defn extract-defendant [s]
  (let [
        defendant-indicators
        ["(?<=sprawy z wniosku?\\</xText\\>)"
         "(?<=sprawy z wniosku?)"
         "(?<=sprawy z wniosku)"
         "(?<=przeciwko\\</xText\\>)"
         "(?<=przeciwko)"
         "(?<=z wniosku)"
         "(?<=sprawy?:?\\</xText\\>)"
         "(?<=s p r a w y)"
         "(?<=sprawy)"]
        defendant-end-indicators
        ["((?!oskarżon)[\\s\\S])*(?=oskarżon)"
         "((?!z powodu apelacji)[\\s\\S])*\\>(?=z powodu apelacji)"
         ;"((?!na skutek apelacji)[\\s\\S])*\\>(?=na skutek apelacji)"
         "((?!przy udziale)[\\s\\S])*(?=przy udziale)"
         "((?!\\>o )[\\s\\S])*\\>(?=o )"
         "((?!\\>o\\<)[\\s\\S])*\\>(?=o)"
         "((?!\\>z )[\\s\\S])*\\>(?=z )"
         "((?!\\>z\\<)[\\s\\S])*\\>(?=z)"
         "((?!na skutek apelacji)[\\s\\S])*\\>(?=na skutek apelacji)"
         "((?!\\>w [^\\<])[\\s\\S])*\\>(?=w )"
         "((?! w [^\\<])[\\s\\S])*(?= w )"]

        ; extracting defendant to nearest </xText>
        ;match (get-first-regex-match defendant-indicators to-next-xText s)

        ; extracting defendant to certain phrases
        match (get-first-defendant-end-indicator-match
               defendant-indicators defendant-end-indicators s)
        ]
    (when
      (not-nil? match)
      (if
        (string? match)
        (let [
              match (identify-defendant match)
              point-indicator (re-find #"\>1[\.\<]" match)
              ]
          (if (nil? point-indicator)
            (handle-defendant match)
            (extract-multiple point-indicator match "oskarżon" s)))
        (handle-defendant (first match))))))

(defn extract-parties-osp [s]
  (let [
        whatever "[\\s\\S]*"
        without-lex-links (remove-xLexLink-tags s)
        match
          (get-first-regex-match-case-ins
           ["(?<=przy udziale)((?!\\</xText\\>)[\\s\\S])*prok"
            "(?<=przy udziale)((?!\\</xText\\>)[\\s\\S])*oskarżyciela"
          "(?<=przy udziale)((?!\\</xText\\>)[\\s\\S])*przedstawiciela urzędu"
            "prokurator prokuratury"
            "prokuratora prok"
            "prokurator[^i]"
            "(?<=sprawy z wniosku\\</xText\\>)"
            "(?<=sprawy z wniosku)"
            "(?<=spraw z wniosków\\</xText\\>)"
            "(?<=spraw z wniosków)"
            "(?<=sprawy z powództwa\\</xText\\>)"
            "(?<=sprawy z powództwa)"
            "(?<=spraw z powództw\\</xText\\>)"
            "(?<=spraw z powództw)"
            "(?<=sprawy z odwołania\\</xText\\>)"
            "(?<=sprawy z odwołania)"
            "(?<=spraw z odwołań\\</xText\\>)"
            "(?<=spraw z odwołań)"
            "(?<=z powództwa\\</xText\\>)"
            "(?<=z powództwa)"
            "(?<=z powództw\\</xText\\>)"
            "(?<=z powództw)"
            "(?<=z odwołania\\</xText\\>)"
            "(?<=z odwołania)"
            "(?<=z odwołań\\</xText\\>)"
            "(?<=z odwołań)"
            "(?<=z wniosku\\</xText\\>)"
            "(?<=z wniosku)"
            "(?<=z wniosków\\</xText\\>)"
            "(?<=z wniosków)"
            "(?<=w sprawie ze skargi)"
            "(?<=po rozpoznaniu w sprawie)"
            "(?<=w obecności oskarżyciela publ)"
            ;"(?<=w obecności)"
            "(?<=\\<xText\\>sprawy\\</xText\\>)"
            "(?<=\\<xText\\>sprawy)"
            "(?<=\\<xBx\\>sprawy\\</xText\\>)"
            "(?<=\\<xBx\\>sprawy)"
            "(?<=w sprawie\\</xText\\>)"
            "(?<=w sprawie)"]
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
        (zipmap
          [:plaintiff :defendant :id]
          [nil nil id])
        (let [
              plaintiff (extract-plaintiff match)
              defendant (extract-defendant match)
              ]
          (zipmap
           [:plaintiff
           :defendant
           ;:txt
           :id]
           (if
             (or
              (nil? defendant)
              (= "<xText></xText>" defendant))
             ["" plaintiff id]
             [plaintiff defendant id]))))
      (prn id))))

(defn dexmlise-cleanse [s]
  (when (not-nil? s)
    (str/trim (dexmlise s))))

(defn dexmlise-parties-osp [coll]
  (map
    #(if (map? %)
      (zipmap
      [:plaintiff
       :defendant
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

(defn remove-xTexts [s]
  (str/replace s #"\<xText((?!\>)[\s\S])*/\>" ""))

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
            (filter #(matches? (str %) re)
                    file-paths)
        ]
    file-paths))

(defn take-to-regex [s re]
  (first
   (str/split s re)))

(defn contains-some [s coll]
  (some
   #(substring? % s)
   coll))

; removing <xText/> tags
(defn get-judgments [file-paths]
  (let [
          files
            (map
              #(slurp %)
              file-paths)
          ss
            (map
              #(remove-xTexts %)
              files)
          judgments (map #(extract-osp-judgments %) ss)
        ]
    judgments))

(def test-set-xml-path "test-data/osp-parties/test-set.xml")

(defn extract-osp-test-xml []
  (let [
          ids-file (slurp "test-data/osp-parties/answers-1.txt")
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
             "/home/floydian/icm/osp/base/"
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
             "/home/floydian/icm/osp/base/"
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
