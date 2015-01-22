(ns saos-tm.extractor.osp-parties
  (:require
    [ clojure.string :as str ]
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

                        #"\>\s\<" "><"

                        #"(?<=[^A-Z])\.\s*$" ""
                        #";\s*$" ""
                        #",\s*$" ""
                        #"'" "\""

                        #"z odwołania" ""
                        #"z powództwa" ""
                        #"powództwa" ""
                        #"z wniosku" ""
                        #"wnioskodawc[^\s]*" ""
                        #"w wniosku" ""
                        #"stronie pozwanej" ""
                        #"pozwan[^\s]*" ""

                        #"oskarżon[^\s]* o czyny? z" ""
                        #"oskarżon[^\s]* z" ""
                        #"oskarżon[^\s]*" ""

                        #"skazan[^\s]*" ""
                        #"obwinione[^\s]*" ""
                        #"skarga" ""
                        #"^ie" ""
                        #"^y" ""
                        #"^u\s" ""
                        #"^\s*:" ""
                        #"­\." ""

                        #"\s+" " "
                        #" +" " "
                        #"[^\S]+" " ")))))

(defn extract-multiple [point-indicator defendant-match splitter s]
  (let [
        points-indicator-strs (str/split point-indicator #"1")
        to-first-point
          (first
           (str/split
            defendant-match
            (re-pattern
             (str "\\" (first points-indicator-strs)
                  "1"
                  "\\" (second points-indicator-strs)
                  ))))
        points-indicator-str
          (str "\\" (first points-indicator-strs)
               "\\d"
               "\\" (second points-indicator-strs)
               )
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
     defendants-in-one-str))

(def point-indicator-regex #"\>\s*1[\.\)]")

(defn extract-plaintiff [s]
  (let [
        plaintiff-match
          (first
           (str/split
            s
            (re-pattern
             (str "po rozpoznaniu|"
                  "przy udziale|"
                  "z udziałem|"
                  "przeciwko|"
                  "obwinion|"
                  "od decyzji"))))
        point-indicator (re-find point-indicator-regex plaintiff-match)
        ]
    (if (nil? point-indicator)
      plaintiff-match
      (extract-multiple
       point-indicator plaintiff-match "</strong>|</p>" s))))

(defn identify-defendant [s]
  (first
    (str/split
      s
     (re-pattern
      (str
       "oskarżon[^\\s]*|"
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
              point-indicator (re-find point-indicator-regex match)
              ]
          (if (nil? point-indicator)
            match
            (extract-multiple point-indicator match "oskarżon" s)))
        (identify-defendant (first match))))))

(defn extract-parties-osp [s]
  (let [
        whatever "[\\s\\S]*"
        match
          (second
           (get-closest-regex-match-case-ins
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

             "(?<=\\>sprawy( z wniosku)?)"
             "(?<=z powództwa)"
;;           "(?<=z powództw)"
             "(?<=z odwołania)"
             "(?<=z odwołań)"
             "(?<=z wniosku)"
             "(?<=z wniosków)"
             "(?<=w sprawie ze skargi)"
             "(?<=po rozpoznaniu w sprawie)"
             "(?<=w obecności oskarżyciela publ)"
             "(?<=w sprawie)"]
            whatever s))
        match
          (when (not-nil? match)
            (str/replace
             match
             #"^\s*z\s*(wniosku|powództwa|odwołania)" ""))
        ]
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
         [:plaintiff :defendant]
         (if
           (nil? defendant)
           ["" (cleanse-party (identify-defendant plaintiff))]
           [(cleanse-party plaintiff) (cleanse-party defendant)]))))))

(defn extract-sentence [s]
  (let [
        s (str/replace s #" " " ")
        match (re-find #"(?i)\>\s*WYROK\s*\<[\s\S]*\>\s*UZASADNIENIE\s*\<" s)
        ]
    (if (nil? match) s match)))

(defn extract-parties-from-judgments [judgments]
  (remove nil?
          (map
           #(extract-parties-osp (extract-sentence %))
           judgments)))
