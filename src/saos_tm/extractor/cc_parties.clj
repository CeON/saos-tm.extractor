(ns saos-tm.extractor.cc-parties
  (:require
   [ clojure.string :as str ]
   [ saos-tm.extractor.common :refer :all]
   [ langlab.core.parsers :refer :all ])
  (:import java.io.File)
  (:gen-class))

(defn cleanse-party [s]
  (when
    (not-nil? s)
    (let [
          without-html-tags (remove-html-tags-other-than-span s)
          s-cleaned
            (str/trim
              (replace-several without-html-tags
                #"[^-]-$" ""
                #"\s+[a-z]\s*$" ""

                (re-pattern system-newline) ""

                #"\>\s\<" "><"

                #"(?<=[^A-Z])\.\s*$" ""

                #";\s*$" ""
                #",\s*$" ""
                #"[^a-żA-Ż0-9><\)\.-]+\s*$" ""

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
                #"[^\S]+" " "))
          ]
      (if (empty? s-cleaned)
        nil
        s-cleaned))))

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
                  "(?i)od\\s+decyzji|"
                  "(?i)od\\s+orzeczenia|"
                  "-? sygn"))))

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
       "przy?\\s+udzial")))))

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
           "(?i)(?<=sprawy\\sz\\spowództwa)"
           "(?i)(?<=sprawy\\sz\\swniosku?\\</p\\>)"
           "(?i)(?<=sprawy\\sz\\swniosku?)"
           "(?i)(?<=sprawy\\sz\\swniosku)"

           "(?i)(?<=sprawy\\sz\\sodwołania)"

           "(?i)(?<=przeciwko\\</p\\>)"
           "(?i)(?<=przeciwko)"
           "(?i)(?<=z\\swniosku)"
           "(?i)(?<=\\>sprawy)"
           "(?i)(?<=sprawy?:?\\</p\\>)"
           "(?i)(?<=s p r a w y)"
           "(?i)(?<=sprawy)"
           "(?i)(?<=sprawę)"
           "(?i)(?<=sprawie)"

           "(?i)(?<=z\\sudziałem\\sprzeciwnika\\sskargi)"
           "(?i)(?<=na\\sskutek\\sodwołania(\\sod\\sdecyzji)?)"

           "(?i)(?<=od\\sdecyzji)"
           "(?i)(?<=od\\sorzeczenia)"
           ]

        defendant-end-indicators
          [
           "(?i)((?!oskarżon)[\\s\\S])*(?=oskarżon)"
           "(?i)((?!obwinion)[\\s\\S])*(?=obwinion)"

           (str "(?i)((?!z\\spowodu\\sapelacji)[\\s\\S])*"
                "\\>(?=z\\spowodu\\sapelacji)")

           (str "(?i)((?!na\\sskutek\\sapelacji)[\\s\\S])*"
                "\\>(?=na\\sskutek\\sapelacji)")

           "(?i)((?!przy\\sudziale)[\\s\\S])*(?=przy\\sudziale)"

           "(?i)((?!\\>o\\s)[\\s\\S])*\\>(?=o\\s)"
           "(?i)((?!\\>o\\<)[\\s\\S])*\\>(?=o)"

           "(?i)((?!\\so\\s)[\\s\\S])*(?=\\so\\s)"

           "(?i)((?!zapłatę)[\\s\\S])*(?=zapłatę)"
           "(?i)((?!w\\sprzedmiocie)[\\s\\S])*(?=w\\sprzedmiocie)"
           "(?i)((?!w\\ssprawie)[\\s\\S])*(?=w\\ssprawie)"
           "(?i)((?!z\\sdnia)[\\s\\S])*(?=z\\sdnia)"

           "(?i)((?!zasądza)[\\s\\S])*(?=zasądza)"
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
        without-hard-spaces (remove-hard-spaces s)
        without-double-spaces (str/replace without-hard-spaces #"\s+" " ")
        match
          (re-find
           #"(?i)\>\s*WYROK\s*\<[\s\S]*\>\s*UZASADNIENIE\s*\<"
           without-double-spaces)
        ]
    (if (nil? match) s match)))

(defn preprocess-cc-parties [s]
  (replace-several s
                   #"<p>|</p>" " "
                   (re-pattern system-newline) " "
                   #"\s+" " "))

(defn extract-parties-cc-criminal [s]
  (let [
        sentence (extract-sentence s)
        sentence-preprocessed (preprocess-cc-parties sentence)
        whatever "[\\s\\S]*"
        closest-match-with-position
          (get-closest-regex-match-case-sen
            [
             "(?i)(?<=przy\\sudziale)"
             "(?i)(?<=z\\sudziałem)"
             "(?i)prokurator:"
             "(?i)prokuratora?\\s+prok"
             "[p|P]rokuratora?\\s+[A-Z]"
             "[p|P]rokuratur[^\\s]*\\s+[A-Z]"
             "[p|P]rokurator\\s–\\s"

             "(?<=w\\sobecności)\\soskarżyciela\\spubl"
             "(?<=w\\sobecności)"
             "(?i)oskarżyciela?\\s+publ"
             "(?i)oskarżyciela?\\s+prywatn"

             "(?i)(?<=sprawy\\sz\\soskarżenia)"
             "(?i)(?<=sprawy\\skarnej\\sz\\soskarżenia)"
             "(?i)(?<=sprawy\\scywilnej\\sz\\soskarżenia)"
             "(?i)(?<=z\\soskarżenia)"
             ]
            whatever sentence-preprocessed)
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
    (if
      (and
       (not= prosecutor "")
       (not-nil? prosecutor))
    { :prosecutor
      (if (matches? prosecutor #"^publiczn[\s\S]*|^subsydiarn[\s\S]*")
        (str "z oskarżenia " prosecutor)
        prosecutor) }
    {})))

(defn is-appeal-case? [defendant match]
  (if (nil? defendant)
    false
    (let [
           defendant-to-parantheses
             (first (str/split defendant #"\(|\)"))
           parts
             (str/split match (re-pattern defendant-to-parantheses))
          ]
      (matches? (first parts) #"[\s\S]*odwoł[^\s]*$"))))

(def ^:private parties-cc-civil-regexs
   [ "(?<=sprawy\\sz\\sodwołania)"

     "(?<=sprawy\\sz\\swniosk)"
     "(?<=spraw\\sz\\spowództw)"
     "(?<=sprawy\\sz\\spowództwa)"
     "(?<=spraw\\sz\\sodwołań)"

      "(?<=powodowi)"
      "(?<=powodom)"
      "(?<=powódce)"
      "(?<=powód(ztw)?)"

      "(?<=z\\spowództwa)"
      "(?<=z\\spowództw)"
      "(?<=z\\sodwołania)"
      "(?<=odwołania)"
      "(?<=z\\sodwołań)"
      "(?<=z\\swniosku)"
      "(?<=z\\swniosków)"
      "(?<=ze\\sskargi)"
      "(?<=ze\\sskarg)"

      "(?<=\\>sprawy\\sz\\swniosku)"
      "(?<=w\\ssprawie\\sze\\sskargi)"
      "(?<=sprawy\\sze\\sskargi)"
      "(?<=po\\srozpoznaniu\\sw\\ssprawie)"

      "(?<=\\>sprawy)"
      "(?<=w\\ssprawie)"
      "(?<=sprawy)"])

(defn extract-parties-cc-civil [s]
  (let [
        sentence (extract-sentence s)
        whatever "[\\s\\S]*"
        match
          (second
            (get-closest-regex-match-case-ins
              parties-cc-civil-regexs
              whatever sentence))
        match-cleaned
          (when match
            (replace-several
             match
             #"^\s*z\s*(wniosku|powództwa|odwołania)" ""
             #"^\s*ze\s*skarg[^\s]*" ""
             #"^a:?\s" ""
             #"^a:?<" "<"
             #"^ztwa:?\s" ""
             #"^\s*obwinion[^\s]*" ""
             #"^ztwa:?<" "<"))
        [plaintiff defendant]
          (if match
            [ (extract-plaintiff match-cleaned)
              (extract-defendant match-cleaned) ]
            [ nil nil ])
       ]
       (if (is-appeal-case? defendant match-cleaned)
          { :plaintiff (cleanse-party defendant)
            :defendant (cleanse-party plaintiff)}
          { :plaintiff (cleanse-party plaintiff)
            :defendant (cleanse-party defendant)})))
