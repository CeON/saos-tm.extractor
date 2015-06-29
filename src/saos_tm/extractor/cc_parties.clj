(ns saos-tm.extractor.cc-parties
  "Module contains algorithm for extraction of parties
  in Polish case law - specifically Common Courts."
  (:require
   [saos-tm.extractor.common :as common]
   [clojure.string :as str]
   [langlab.core.parsers :as langlab-parsers])
  (:import java.io.File)
  (:gen-class))

(def ^:private defendant-indicators
  ["(?i)(?<=sprawy\\sz\\spowództwa)"
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
   "(?i)(?<=od\\sorzeczenia)"])

(def ^:private defendant-end-indicators
  ["(?i)((?!oskarżon)[\\s\\S])*(?=oskarżon)"
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

   "(?i)((?!zasądza)[\\s\\S])*(?=zasądza)"])

(def ^:private point-indicator-regex #"(?<=>)\s*\d+[\.\)]")

(def ^:private parties-cc-criminal-indicators
  ["(?i)(?<=przy\\sudziale)"
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
   "(?i)(?<=z\\soskarżenia)"])

(def ^:private parties-cc-civil-indicators
  ["(?<=sprawy\\sz\\sodwołania)"

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

(defn ^:private cleanse-party [s]
  (when
    (common/not-nil? s)
    (let [
          without-html-tags (common/remove-html-tags-other-than-span s)
          s-cleaned
            (str/trim
              (common/replace-several without-html-tags
                #"[^-]-$" ""
                #"\s+[a-z]\s*$" ""

                (re-pattern common/system-newline) ""

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
      (when-not (empty? s-cleaned) s-cleaned))))

(defn ^:private extract-multiple [match splitter s]
  (let [
        to-first-point
          (first
           (str/split
            match
            point-indicator-regex))
        party-entities (langlab-parsers/split* match point-indicator-regex)
        party-entities
          (map
           #(first (str/split % splitter))
           party-entities)
        all
          (cleanse-party
           (apply str (map #(str %1 %2) party-entities (repeat " "))))
        ]
    all))

(defn ^:private extract-plaintiff [s]
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

(defn ^:private identify-defendant [s]
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

(defn ^:private get-first-defendant-end-indicator-match
  [defendant-end-indicators s]
  (let [
        matches
          (map
           #(common/get-closest-regex-match defendant-indicators % s)
           defendant-end-indicators)
        matches (sort #(compare (first %1) (first %2)) matches)
        match
          (second
           (common/find-first
            #(common/not-nil? %)
            matches))
        match
          (if (string? match)
            match
            (first match))
        ]
    match))

(defn ^:private extract-defendant-str-match [match s]
  (let [
        defendant (identify-defendant match)
        point-indicator (re-find point-indicator-regex defendant)
        ]
    (if (nil? point-indicator)
      match
      (extract-multiple match #"oskarżon" s))))

(defn ^:private extract-defendant [s]
  (let [
        ; extracting defendant to certain phrases
        match
          (get-first-defendant-end-indicator-match defendant-end-indicators s)
        match-cleansed
          (when (common/not-nil? match)
            (common/replace-several match #"^\s*przeciw[^\s]*" ""))
        ]
    (when
      (common/not-nil? match-cleansed)
      (if
        (string? match-cleansed)
        (extract-defendant-str-match match-cleansed s)
        (identify-defendant (first match))))))

(defn ^:private extract-sentence [s]
  (let [
        without-hard-spaces (common/remove-hard-spaces s)
        without-double-spaces (str/replace without-hard-spaces #"\s+" " ")
        match
          (re-find
           #"(?i)\>\s*WYROK\s*\<[\s\S]*\>\s*UZASADNIENIE\s*\<"
           without-double-spaces)
        ]
    (if (nil? match) s match)))

(defn ^:private preprocess-cc-parties [s]
  (common/replace-several s
                          #"<p>|</p>" " "
                          (re-pattern common/system-newline) " "
                          #"\s+" " "))

(defn extract-cc-parties-criminal
  "Extracts parties that occurr in a given string `s` which is assumed
  to be a criminal case of Common Polish Courts.
  It works and is tested on texts with html tags.

  The result is a map containing keys:

  * `:prosecutor` - name of prosecutor

  Example:

  `(extract-cc-parties-criminal \"<p>Protokolant Agnieszka Malewska</p>
  <p>przy udziale Prokuratora Prokuratury Okręgowej w Białymstoku
  Wiesławy Sawośko-Grębowskiej</p> <p>po rozpoznaniu
  w dniu 28 marca 2013 roku</p>\")`

  `{:prosecutor \"Prokuratora Prokuratury Okręgowej w Białymstoku
  Wiesławy Sawośko-Grębowskiej\"}`

  Functions works in an analogous way to extract-cc-parties-civil.
  At first the function extracts sentence of judgment.
  It uses `parties-cc-criminal-indicators` to localize the place of parties
  appearance in text. Then it extracts prosecutor with `extract-plaintiff`
  function, because the same function can be used for civil and criminal
  cases in this context.
  "
  [s]
  (let [
        sentence (extract-sentence s)
        sentence-preprocessed (preprocess-cc-parties sentence)
        whatever "[\\s\\S]*"
        closest-match-with-position
          (common/get-closest-regex-match-case-sen
            parties-cc-criminal-indicators
            whatever sentence-preprocessed)
        match (second closest-match-with-position)
        prosecutor
          (if
            (or
             (nil? match)
             (common/matches?
              match #"(?i)^prokurator[^\s]*\s+rejonow[^\s]*[\S\s]*")
             (common/matches?
              match #"(?i)^prokurator[^\s]*\s+generaln[^\s]*[\S\s]*"))
            ""
            (cleanse-party (extract-plaintiff match)))
        ]
    (if
      (and
       (not= prosecutor "")
       (common/not-nil? prosecutor))
    { :prosecutor
      (if (common/matches? prosecutor #"^publiczn[\s\S]*|^subsydiarn[\s\S]*")
        (str "z oskarżenia " prosecutor)
        prosecutor) }
    {})))

(defn ^:private is-appeal-case? [defendant match]
  (if (nil? defendant)
    false
    (let [
           defendant-to-parantheses
             (first (str/split defendant #"\(|\)"))
           parts
             (str/split match (re-pattern defendant-to-parantheses))
          ]
      (common/matches? (first parts) #"[\s\S]*odwoł[^\s]*$"))))

(defn extract-cc-parties-civil
  "Extracts parties that occurr in a given string `s` which is assumed
  to be a civil case of Common Polish Courts.
  It works and is tested on texts with html tags.

  The result is a map containing keys:

  * `:plaintiff` - name (often partly of fully anonymized) of plaintiff
  (can be a person of organization)
  * `:defendant` - name (often partly of fully anonymized) of defendant
  (can be a person of organization)

  Example:

  `(extract-cc-parties-civil \"<strong><!-- -->sprawy z wniosku
  <span class=\"anon-block\">Ł. G.</span> </strong></p>
  <p>przeciwko Zakładowi Ubezpieczeń Społecznych Oddział w
  <span class=\"anon-block\">O.</span></p> <p>
  o prawo do renty z tytułu niezdolności do pracy</p>\")`

  `{:plaintiff \"<span class=\"anon-block\">Ł. G.</span>\",
  :defendant \"Zakładowi Ubezpieczeń Społecznych
  Oddział w <span class=\"anon-block\">O.</span>\"}`

  At first the function extracts sentence of judgment.
  It uses `parties-cc-civil-indicators` to localize the place of parties
  appearance in text. Then it extracts plaintiff and defendant with
  `extract-plaintiff` and `extract-defendant' functions respectively.
  "
  [s]
  (let [
        sentence (extract-sentence s)
        whatever "[\\s\\S]*"
        match
          (second
           (common/get-closest-regex-match-case-ins
            parties-cc-civil-indicators
            whatever sentence))
        match-cleansed
          (when match
            (common/replace-several
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
            [ (extract-plaintiff match-cleansed)
              (extract-defendant match-cleansed) ]
            [ nil nil ])
        ]
    (if (is-appeal-case? defendant match-cleansed)
      { :plaintiff (cleanse-party defendant)
        :defendant (cleanse-party plaintiff)}
      { :plaintiff (cleanse-party plaintiff)
        :defendant (cleanse-party defendant)})))
