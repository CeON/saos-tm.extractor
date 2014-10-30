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

(defn extract-plaintiff [s]
  (first
    (str/split s #"\</xText\>")))

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

(defn extract-defendant [s]
  (let [
        to-next-xtext "((?!\\</xText\\>)[\\s\\S])*(?=\\</xText\\>)"
        match
          (get-first-regex-match
            ["(?<=sprawy( z wniosku)?\\</xText\\>)"
             "(?<=sprawy( z wniosku)?)"
             "(?<=sprawy z wniosku)"
             "(?<=przeciwko\\</xText\\>)"
             "(?<=przeciwko)"
             "(?<=z wniosku)"]
             to-next-xtext s)
        ]
        (when
          (not-nil? match)
          (if
            (string? match)
            match
            (first match)))))

(defn cleanse-party [s]
  (when
    (not-nil? s)
    (str/trim
      (replace-several s
        (re-pattern system-newline) ""
        #"\s+" " "
        #"z odwołania" ""
        #"z powództwa" ""
        #"z wniosku" ""
        #"wnioskodawc(zyni|y|ów:)" ""
        #"^\s*:" ""
        ;#"^\s*\((\.)*\)" ""
        ))))

(defn extract-parties-osp [s]
  (let [
          whatever "[\\s\\S]*"
          regex-str
            (str "(?i)(?<=przy udziale) prok" whatever "|"
                 "(?i)prokurator prokuratury" whatever "|"
                 "(?i)prokuratora prok" whatever "|"
                 "(?<=(?i)sprawy z wniosku)" whatever "|"
                 "(?<=(?i)sprawy z powództwa)" whatever "|"
                 "(?<=(?i)sprawy z odwołania)" whatever "|"
                 "(?<=(?i)z powództwa)" whatever "|"
                 "(?<=(?i)w sprawie ze skargi)" whatever "|"
                 "(?<=(?i)po rozpoznaniu w sprawie)" whatever "|"
                 "(?<=(?i)<xText>sprawy)" whatever)
          result
            (re-find
              (re-pattern regex-str)
              s)
    ]
    (if
      (nil?
        (re-find
          (re-pattern
            (str
              "(?i)\\<xName\\>postanowienie\\</xName\\>|"
              "(?i)\\<xName\\>uzasadnienie\\</xName\\>"))
         s))
      (if
        (nil? result)
        (re-find #"id=.\d+[_a-zA-Z0-9\-]+" s)
        (zipmap
          [:plaintiff :defendant]
          [(cleanse-party (extract-plaintiff result)) 
           (cleanse-party (extract-defendant result))])))))

(defn dexmlise-cleanse [s]
  (when (not-nil? s)
    (str/trim (dexmlise s))))

(defn dexmlise-parties-osp [coll]
  (map
    #(if (map? %)
      (zipmap
      [:plaintiff :defendant]
      [(dexmlise-cleanse (:plaintiff %))
       (dexmlise-cleanse (:defendant %))])
      %)
    coll))

(defn join-newline [coll]
  (clojure.string/join
      system-newline
      coll))

(defn remove-xtexts [s]
  (str/replace s #"\<xText/\>" ""))

(defn extract-parties-from-txt [s]
  (dexmlise-parties-osp
    ;(remove nil?
      (map
        #(extract-parties-osp
          (first
            (split-osp-judgment-to-parts %)))
        (extract-osp-judgments s))))

(defn count-osp-judgments [s]
  (count
    (extract-osp-judgments s)))

(defn spit-parties []
  (let [
          file-paths
            (.listFiles
              (clojure.java.io/file "/home/floydian/icm/osp/base/"))
          file-paths
            (take 10
              (filter-ending-with file-paths "_con.xml"))
          files
            (map
              #(slurp %)
              file-paths)
          ss
            (map
              #(remove-xtexts %)
              files)
          judgments-counts
            (map
              #(count-osp-judgments %)
              ss)
          judgments-count
            (reduce + judgments-counts)
          osp-parties
            (mapcat
              #(extract-parties-from-txt %)
              ss)
          ids-not-extracted
            (filter
              #(string? %)
              osp-parties)
          _ (prn (count ids-not-extracted))
          _ (prn ids-not-extracted) 
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
          _ (prn judgments-count)
    ]
    (spit "defendants.txt"
      (join-newline defendants))
    (spit "plaintiffs.txt"
      (join-newline plaintiffs))))

(defn get-osp-judgment-by-id [id s]
  (apply str
    (filter
      #(not-nil?
        (re-find (re-pattern id) %))
      (extract-osp-judgments s))))