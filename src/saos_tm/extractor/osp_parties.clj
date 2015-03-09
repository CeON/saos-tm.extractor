(ns saos-tm.extractor.osp-parties
  (:require
   [ clojure.string :as str ]
   [saos-tm.extractor.common :refer :all]
   [ langlab.core.parsers :refer :all ])
  (:import java.io.File)
  (:gen-class))

(defn cleanse-party [s]
  (when
    (not-nil? s)
    (let [
          without-html-tags (remove-html-tags-other-than-span s)
          ]
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

                        #"[^\S]+" " ")))))

(def point-indicator-regex #"(?<=>)\s*\d+[\.\)]")

(defn extract-multiple [match splitter s]
  (let [
        to-first-point
          (first
           (str/split
            match
            point-indicator-regex))
        party-entities (split* match point-indicator-regex)
        party-entities
          (map
           #(first (str/split % splitter))
           party-entities)
        all
          (cleanse-party
           (apply str (map #(str %1 %2) party-entities (repeat " "))))
        ]
    all))

(defn extract-plaintiff [s]
  (let [
        plaintiff-match
          (first
           (str/split
            s
            (re-pattern
             (str "(?i)po\\s+rozpoznaniu|"
                  "(?i)przy\\s+udziale|"
                  "(?i)przy\\s+uczestnictwie|"
                  "(?i)z\\s+udziałem|"
                  "(?i)z\\s+urzędu|"
                  "(?i)przeciwko|"
                  "(?i)obwinion|"
                  "(?i)oskarżon|"
                  "(?i)ukarane|"
                  "(?i)skazan|"
                  "(?i)spraw|"
                  "(?i)rozpozn|"
                  "(?i)\\s+o\\s+|"
                  "(?i)(?<=>)o\\s+|"
                  "(?i)(?<=>)o(?<=<)|"
                  "(?i)prowadząc[^\\s]+\\s+działaln[^\\s]+|"
;;                   "(?i) przez |"
                  "(?i)od\\s+decyzji"))))
        point-indicator (re-find point-indicator-regex plaintiff-match)
        ]
    (if (nil? point-indicator)
      plaintiff-match
      (extract-multiple plaintiff-match #"</strong>|</p>" s))))

(defn identify-defendant [s]
  (first
    (str/split
      s
     (re-pattern
      (str
       "oskarżon[^\\s]*|"
       "\\s+o\\s+|"
       "w\\s+przedmiocie|"
       "-o\\s+|"
       "osk\\.\\s+z|"
       "o\\s+zasądzenie|"
       "o\\s+odszkodowanie|"
       "odszkodowanie|"
       "zapłat|"
       "obwinion|"
       "pozwan|"
       "z\\s+udziałem|"
       "przy?\\s+udzial|"
       "prowadząc[^\\s]+\\s+działaln[^\\s]+")))))

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
          [
           "(?i)(?<=sprawy\\s\\s?z\\s\\s?powództwa)"
           "(?i)(?<=sprawy\\s\\s?z\\s\\s?wniosku?\\</p\\>)"
           "(?i)(?<=sprawy\\s\\s?z\\s\\s?wniosku?)"
           "(?i)(?<=sprawy\\s\\s?z\\s\\s?wniosku)"
           "(?i)(?<=przeciwko\\</p\\>)"
           "(?i)(?<=przeciwko)"
           "(?i)(?<=z\\s\\s?wniosku)"
           "(?i)(?<=\\>sprawy)"
           "(?i)(?<=sprawy?:?\\</p\\>)"
           "(?i)(?<=s p r a w y)"
           "(?i)(?<=sprawy)"
           "(?i)(?<=sprawę)"
           "(?i)(?<=sprawie)"
           ]
        defendant-end-indicators
          [
           "(?i)((?!oskarżon)[\\s\\S])*(?=oskarżon)"
           "(?i)((?!obwinion)[\\s\\S])*(?=obwinion)"

           (str "(?i)((?!z\\s\\s?powodu\\s\\s?apelacji)[\\s\\S])*"
                "\\>(?=z\\s\\s?powodu\\s\\s?apelacji)")

           (str "(?i)((?!na\\s\\s?skutek\\s\\s?apelacji)[\\s\\S])*"
                "\\>(?=na\\s\\s?skutek\\s\\s?apelacji)")

           "(?i)((?!przy\\s\\s?udziale)[\\s\\S])*(?=przy\\s\\s?udziale)"

           "(?i)((?!\\>o\\s\\s?)[\\s\\S])*\\>(?=o\\s\\s?)"
           "(?i)((?!\\>o\\<)[\\s\\S])*\\>(?=o)"
           "(?i)((?!\\s\\s?o\\s\\s?)[\\s\\S])*\\>(?=\\s\\s?o\\s\\s?)"
           ]

        ; extracting defendant to certain phrases
        match (get-first-defendant-end-indicator-match
               defendant-indicators defendant-end-indicators s)

        match
          (when (not-nil? match)
            (replace-several
             match
             #"^\s*przeciw[^\s]*" ""))
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
            (extract-multiple match #"oskarżon" s)))
        (identify-defendant (first match))))))

(defn extract-sentence [s]
  (let [
        s (remove-hard-spaces s)
        match (re-find #"(?i)\>\s*WYROK\s*\<[\s\S]*\>\s*UZASADNIENIE\s*\<" s)
        ]
    (if (nil? match) s match)))

(defn no-plaintiff? [match]
  (or
   (nil? match)
   (matches? match #"(?i)^prokurat[^\s]*\s+rejonow[^\s]*[\S\s]*")
   (matches? match #"(?i)^prokurat[^\s]*\s+generaln[^\s]*[\S\s]*")))

(defn extract-parties-osp-criminal [s]
  (let [
        sentence (extract-sentence s)
        whatever "[\\s\\S]*"
        closest-match-with-position
          (get-closest-regex-match-case-sen
            [
             "(?i)(?<=przy\\s\\s?udziale)"
             "(?i)prokurator:"
             "(?i)prokuratora?\\s+prok"
             "[p|P]rokuratora?\\s+[A-Z]"
             "[p|P]rokuratur[^\\s]*\\s+[A-Z]"
             "(?<=w\\s\\s?obecności)\\s\\s?oskarżyciela\\s\\s?publ"
             "(?<=w\\s\\s?obecności)"
             "(?i)oskarżyciela?\\s+publ"
             "(?i)oskarżyciela?\\s+prywatn"
             ]
            whatever sentence)
        match (second closest-match-with-position)
        prosecutor
          (if
            (or
             (nil? match)
             (matches? match #"(?i)^prokurator[^\s]*\s+rejonow[^\s]*[\S\s]*")
             (matches? match #"(?i)^prokurator[^\s]*\s+generaln[^\s]*[\S\s]*"))
            ""
            (cleanse-party (extract-plaintiff match)))
        ]
    (zipmap
     [:prosecutor :defendant]
     [prosecutor
      "" ;; we don't want to extract defendant for criminal cases
      ])))

(defn extract-parties-osp-civil [s]
  (let [
        sentence (extract-sentence s)
        whatever "[\\s\\S]*"
        match
          (second
           (get-closest-regex-match-case-ins
            [
             "(?<=sprawy\\s\\s?z\\s\\s?odwołania)"
             "(?<=sprawy\\s\\s?z\\s\\s?wniosk)"
             "(?<=spraw\\s\\s?z\\s\\s?powództw)"
             "(?<=sprawy\\s\\s?z\\s\\s?powództwa)"
             "(?<=spraw\\s\\s?z\\s\\s?odwołań)"

             "(?<=powodowi)"
             "(?<=powodom)"
             "(?<=powódce)"
             "(?<=powód(ztw)?)"

             "(?<=z\\s\\s?powództwa)"
             "(?<=z\\s\\s?powództw)"
             "(?<=z\\s\\s?odwołania)"
             "(?<=z\\s\\s?odwołań)"
             "(?<=z\\s\\s?wniosku)"
             "(?<=z\\s\\s?wniosków)"
             "(?<=ze\\s\\s?skargi)"
             "(?<=ze\\s\\s?skarg)"

             "(?<=\\>sprawy\\s\\s?z\\s\\s?wniosku)"
             "(?<=w\\s\\s?sprawie\\s\\s?ze\\s\\s?skargi)"
             "(?<=sprawy\\s\\s?ze\\s\\s?skargi)"
             "(?<=po\\s\\s?rozpoznaniu\\s\\s?w\\s\\s?sprawie)"

             "(?<=\\>sprawy)"
             "(?<=w\\s\\s?sprawie)"
             "(?<=sprawy)"
             ]
            whatever sentence))
        match
          (when (not-nil? match)
            (replace-several
             match
             #"^\s*z\s*(wniosku|powództwa|odwołania)" ""
             #"^a:?\s" ""
             #"^a:?<" "<"
             #"^ztwa:?\s" ""
             #"^ztwa:?<" "<"))
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

(defn extract-parties-from-judgments [judgments extract-parties-func]
  (remove nil?
          (map
           #(extract-parties-func %)
           judgments)))
