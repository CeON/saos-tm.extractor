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
    #"(?<=\>)\n(?=[^\<])" ""
    #"(?<=[^\>])\n(?=\<)" ""))

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

(defn extract-defendant [s]
  (let [
        pattern
          (re-pattern
            (str "(?<=sprawy( z wniosku)?)((?!\\</xText\\>)[\\s\\S])*"
                 "\\</xText\\>|"
                 "(?<=sprawy( z wniosku)?\\</xText\\>)"
                 "((?!\\</xText\\>)[\\s\\S])*\\</xText\\>|"
                 "(?<=przeciwko)((?!\\</xText\\>)[\\s\\S])*(?=\\</xText\\>)"))
        regex-match
          (re-find
            pattern
            s)
          ]
          (when
            (not-nil? regex-match)
            (if
              (string? regex-match)
              regex-match
              (first regex-match)))))

(defn cleanse-party [s]
  (when
    (not-nil? s)
    (str/trim
      (replace-several s
        #"\n" ""
        #"\<((?![\<\>])[\s\S])*\>" ""
        #"\s+" " "
        #"z powództwa" ""
        #"z wniosku" ""
        #"^\s*:" ""
        #"^\s*\((\.)*\)" ""
        ))))

(defn extract-parties-osp [s]
  (let [
          xtexts ".*\n.*\n.*\n.*\n.*\n.*\n.*\n"
          regex-str
            (str "(?<=(?i)przy udziale) prok" xtexts "|"
                 "(?i)prokurator prokuratury" xtexts "|"
                 "(?i)prokuratora prok" xtexts "|"
                 "(?<=(?i)sprawy z wniosku)" xtexts "|"
                 "(?<=(?i)sprawy z powództwa)" xtexts "|"
                 "(?<=(?i)sprawy z odwołania)" xtexts "|"
                 "(?<=(?i)z powództwa)" xtexts "|"
                 "(?<=(?i)w sprawie ze skargi)" xtexts "|"
                 "(?<=(?i)<xText>sprawy)" xtexts)
          result
            (re-find
              (re-pattern regex-str)
              s)
    ]
    (if
      (nil? result)
      (re-find #"id=\"\d+[_a-zA-Z0-9\-]+" s)
      (zipmap
         [
         :plaintiff
         :defendant
         ]
       [
       (cleanse-party (extract-plaintiff result)) 
       (cleanse-party (extract-defendant result))
       ]))))

(defn spit-parties [s]
  (spit "out.xml"
    (clojure.string/join
      (str \newline "====" \newline)
      (map
        #(extract-parties-osp
          (first
            (split-osp-judgment-to-parts %)))
        (extract-osp-judgments s)))))