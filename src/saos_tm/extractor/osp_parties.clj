(ns saos-tm.extractor.osp-parties
  (:require
    [ clojure.string :as str ]
    [ clojure.xml :as xml ]
    [ clojure.zip :as zip ]
    [saos-tm.extractor.common :refer :all])
  (:import java.io.File)
  (:gen-class))

(defn cleanse-party [s]
  (let [
        without-html-tags (remove-html-tags-other-than-span s)
        ]
    (when
      (not-nil? without-html-tags)
      (str/trim
       (replace-several without-html-tags

                        #"[^-]-$" ""
                        #"\s+[a-z]\s*$" ""

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
                        )))))

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
           #(cleanse-party %)
           defendants-complete)
        defendants-in-one-str (apply str defendants-complete)
        ]
    (close-xText-tags defendants-in-one-str)))

(defn extract-plaintiff [s]
  (let [
        plaintiff-match
          (first
           (str/split s #"po rozpoznaniu|przy udziale|przeciwko"))

;;         point-indicator (re-find #"\>1[\.\<]" plaintiff-match)
        ]
;;     (if (nil? point-indicator)
        (cleanse-party plaintiff-match)))
;;     )
;;       (extract-multiple point-indicator plaintiff-match "</xText>" s))))

(defn identify-defendant [s]
;;   (print-if-contains s "A. L.")
  (first
    (str/split
      s
     (re-pattern
      (str
       "oskarżon[^\\s]* z|"
       "\\so\\s|"
       "w przedmiocie|"
       "-o\\s|"
       "osk\\. z|"
       "o zasądzenie|"
       "o odszkodowanie|"
       "odszkodowanie|"
       "zapłat|"
       "obwinion|"
       "prowadzącym działalność")))))

(defn handle-defendant [s]
  (cleanse-party s))

(defn get-first-defendant-end-indicator-match
  [defendant-indicators defendant-end-indicators s]
  (let [
        matches
          (map
           #(get-closest-regex-match defendant-indicators % s)
           defendant-end-indicators)
        matches (sort #(compare (first %1) (first %2)) matches)
        match
          (second
           (find-first
            #(not-nil? %)
            matches))
        match
          (if (string? match)
            match
            (first match))
        ]
    match))

(defn extract-defendant [s]
  (let [
        defendant-indicators
          ["(?<=sprawy z powództwa)"
           "(?<=sprawy z wniosku?\\</p\\>)"
           "(?<=sprawy z wniosku?)"
           "(?<=sprawy z wniosku)"
           "(?<=przeciwko\\</p\\>)"
           "(?<=przeciwko)"
           "(?<=z wniosku)"
           "(?<=\\>sprawy)"
           "(?<=sprawy?:?\\</p\\>)"
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
;;           (if (nil? point-indicator)
            (handle-defendant match)
;;             (extract-multiple point-indicator match "oskarżon" s))
          )
        (handle-defendant (identify-defendant (first match)))))))

(defn extract-parties-osp [s]
  (let [
        whatever "[\\s\\S]*"
;;         without-html-tags (remove-html-tags-other-than-span s)
        match
          (get-first-regex-match-case-ins
           [
            "(?<=przy udziale) prok"
            "(?<=przy udziale) oskarżyciel"
            "(?<=sprawy z odwołania)"
            "(?<=przy udziale)"
            "prokurator prokuratury"
            "prokuratora prok"
            "prokurator[^i]"

            "(?<=sprawy z wniosk)"
            "(?<=spraw z powództw)"
            "(?<=sprawy z powództwa)"
            "(?<=spraw z odwołań)"

            "(?<=\\>sprawy)"
            "(?<=z powództwa)"
            "(?<=z powództw)"
            "(?<=z odwołania)"
            "(?<=z odwołań)"
            "(?<=z wniosku)"
            "(?<=z wniosków)"
            "(?<=w sprawie ze skargi)"
            "(?<=po rozpoznaniu w sprawie)"
            "(?<=w obecności oskarżyciela publ)"
            "(?<=w sprawie)"]
           whatever s)
    ]
    (if
      (nil?
        (re-find
          (re-pattern
            (str
              "(?i)\\<xName\\>\\s*postanowienie.?\\s*\\</xName\\>|"
              "(?i)\\<xName\\>\\s*uzasadnienie.?\\s*\\</xName\\>"))
         s))
      (if
        (nil? match)
        (zipmap
          [:plaintiff :defendant]
          [nil nil])
        (let [
              plaintiff (extract-plaintiff match)
              defendant (extract-defendant match)
              ]
          (zipmap
           [:plaintiff
           :defendant
           ;:txt
           ]
           (if
             (nil? defendant)
             ["" (cleanse-party (identify-defendant plaintiff))]
             [plaintiff defendant])))))))

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

(defn extract-sentence [s]
  (let [
        match (re-find #"(?i)\>\s*WYROK\s*\<[\s\S]*\>\s*UZASADNIENIE\s*\<" s)
        ]
    (if (nil? match)
      s
      match)))

(defn extract-parties-from-judgments [judgments]
  (remove nil?
          (map
           #(extract-parties-osp (extract-sentence %))
           judgments)))
