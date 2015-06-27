(ns saos-tm.extractor.law-links-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [saos-tm.extractor.common  :as common]
            [saos-tm.extractor.law-links :as law-links]
            [langlab.core.parsers :refer [lg-split-tokens-bi]]))

(deftest article-coords-test
  (is(=
      [["4" "0" "0" "2" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 4 pkt 2")))
  (is(=
      [["14a" "0" "0" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 14a ")))
  (is(=
      [["50" "0" "1" "1" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 50 ust. 1 pkt 1 ")))
  (is(=
      [["3" "0" "2-3" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 3 ust. 2-3 ")))
  (is(=
      [["103-105" "0" "0" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 103-105 ")))
  (is(=
      [["47" "0" "1" "2" "0" "0"] ["47" "0" "1" "3" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 47 ust. 1 pkt 2 i 3 ")))
  (is(=
      [["0" "44" "1" "1" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "§ 44 ust. 1 pkt 1 ")))
  (is(=
      [["0" "25" "3" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "§ 25 ust. 3 ")))
  (is(=
      [["0" "68a" "1" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "§ 68a ust. 1 ")))
  (is(=
      [["0" "79" "0" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "§ 79 ")))
  (is(=
      [["0" "34" "3" "2" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "§ 34 ust. 3 pkt 2 ")))
  (is(=
      [["37" "4a" "0" "0" "0" "0"] ["37" "4b" "0" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 37 § 4a, 4b ")))
  (is(=
      [["56" "1-3" "0" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 56 § 1-3 ")))
  (is(=
      [["77" "1" "0" "0" "0" "0"] ["77" "2" "0" "0" "0" "0"]
       ["77" "2a" "0" "0" "0" "0"] ["77" "3" "0" "0" "0" "0"]
       ["77" "3a" "0" "0" "0" "0"] ["77" "6" "0" "0" "0" "0"]
       ["77" "7a" "0" "0" "0" "0"] ["77" "7b" "0" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 77 § 1, 2, 2a, 3, 3a, 6, 7a i 7b ")))
  (is(=
      [["46" "1" "0" "0" "1" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 46 § 1 zd. 1 ")))
  (is(=
      [["178" "0" "1" "0" "0" "0"] ["91" "0" "1" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 178 ust. 1 i art. 91 ust. 1")))
  (is(=
      [["84" "0" "0" "0" "0" "0"] ["92" "0" "1" "0" "0" "0"]
       ["31" "0" "3" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 84, art. 92 ust. 1 i art. 31 ust. 3")))
  (is(=
      [["2" "0" "0" "0" "0" "0"] ["84" "0" "0" "0" "0" "0"]
       ["91" "0" "1" "0" "0" "0"] ["178" "0" "1" "0" "0" "0"]]
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 2, art. 84, z art. 91 ust. 1, art. 178 ust. 1")))
  (is(=
      '(("64" "0" "2" "0" "0" "0") ("64" "0" "3" "0" "0" "0")
        ("84" "0" "0" "0" "0" "0"))
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 64 ust. 2 i 3 oraz art. 84")))
  (is(=
      '(("64" "0" "2" "0" "0" "a"))
      (#'saos-tm.extractor.law-links/extract-art-coords
       "art. 64 ust. 2 lit. a"))))

(deftest article-ranges-test []
  (let [
        tokens
          (lg-split-tokens-bi "pl"
            (str "jest niezgodny z art. 2, art. 31 ust. 3, art. 64 "
                 "ust. 1, 2 i 3, art. 84, art. 91 ust. 1, art. 92 ust. 1, "
                 "art. 178 ust. 1 Konstytucji i art. 13 pkt 1 i 3 aneksu A "
                 "„Ogólne warunki odnoszące się do memorandum finansowego” "
                 "do Umowy Ramowej"))
          ]
          (is (=
              [[3 49][51 58]]
              (#'saos-tm.extractor.law-links/get-correct-art-coords-ranges
               tokens)))))

(defn ^:private get-line-with-signature [s]
  (let [
        lines (str/split s (re-pattern common/system-newline))
        lines-with-sygn-text (filter #(.startsWith % "Sygn.") lines)
        index-of-first-line-ending-with-date
          (first
           (common/indices
            #(.endsWith % " r.")
            lines))
        ]
    (if (empty? lines-with-sygn-text)
      (nth lines (+ index-of-first-line-ending-with-date 1))
      (first lines-with-sygn-text))))

(deftest get-line-with-signature-test []
  (is (=
    "(K. 1/91)"
    (get-line-with-signature
      (str "Orzeczenie\n"
        "z dnia 28 maja 1991 r.\n"
        "(K. 1/91)\n\n"
        "Trybunał Konstytucyjny w składzie:"))))
  (is (=
    "(K. 1/92)"
    (get-line-with-signature
      (str "Orzeczenie\n"
        "z dnia 20 października 1992 r.\n"
        "(K. 1/92)\n\n"
        "Trybunał Konstytucyjny w składzie:"))))
  (is (=
    "Sygn. akt (K. 1 /93)"
    (get-line-with-signature
      (str "Orzeczenie\n"
        "z dnia 22 czerwca 1993 r.\n"
        "Sygn. akt (K. 1 /93)\n\n\n"
        "Trybunał Konstytucyjny w składzie:")))))

(deftest extract-law-links-test []
  (is(=
  '({:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
    :art {:lit "0", :zd "0", :pkt "0", :ust "1-6", :par "0", :art "3"}}
    {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
    :art {:lit "0", :zd "0", :pkt "5", :ust "0", :par "0", :art "4"}}
    {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
    :art {:lit "0", :zd "0", :pkt "6", :ust "0", :par "0", :art "4"}}
    {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
    :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "57"}})
  (:extracted-links (law-links/extract-law-links
    (str "art. 3 ust. 1-6, art. 4 pkt 5 i 6, art. 57 ustawy z dnia 19 grudnia"
         " 2008 r. o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656)")
                     true true true))))
  (is(=
    '({:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "4-6", :par "0", :art "3"}}
      {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "6", :ust "0", :par "0", :art "4"}}
      {:act {:journalEntry "483", :journalNo "78", :journalYear "1997"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "2"}}
      {:act {:journalEntry "483", :journalNo "78", :journalYear "1997"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "1", :par "0", :art "32"}}))
  (:extracted-links (law-links/extract-law-links
    (str "art. 3 ust. 4-6 i art. 4 pkt 6 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656) "
         "z art. 2 i art. 32 ust. 1 Konstytucji")
                     true true true)))
  (is(=
    '({:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}
      {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "5"}}))
  (:extracted-links (law-links/extract-law-links
    (str "art. 3 w związku z art. 5 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656)")
                     true true true)))
  (is(=
    '({:act {:journalNo "16" :journalEntry "93", :journalYear "1964"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}))
  (:extracted-links (law-links/extract-law-links
    (str "art. 3 kc według ustawy o trybunale oprócz kodeksu wykroczeń ")
                     true true true))))

(deftest tokens-to-string-test []
  (let [
          s (str "1) art. 9 ustawy z dnia 27 lipca 2001 r. – "
          "Prawo o ustroju sądów powszechnych "
          "(Dz. U. Nr 98, poz. 1070, ze zm.)"
          " z art. 2, art. 10, art. 45 ust. 1, art. 173, art. 176 ust. 2"
          " i art. 178 ust. 1 Konstytucji Rzeczypospolitej Polskiej;")
    ]
  (is
   (=
    (#'saos-tm.extractor.law-links/tokens-to-string
     (#'saos-tm.extractor.law-links/split-to-tokens s))
    s))))

(defn ^:private extract-law-journal-case-one [s answer]
  (is (=
    (#'saos-tm.extractor.law-links/extract-act-coords
     (#'saos-tm.extractor.law-links/split-to-tokens s)
     [] [] #'saos-tm.extractor.law-links/dictionary-for-acts)
    answer)))

(deftest extract-law-journal-case-test []
  (extract-law-journal-case-one
    (str "ustawy z dnia 1 sierpnia 1997 r. o Trybunale Konstytucyjnym"
      " (Dz.U. Nr 102, poz. 643)")
    {:journalYear "1997" :journalNo "102" :journalEntry "643"})
  (extract-law-journal-case-one
    (str "ustawy z dnia 29 stycznia 2004 r. – Prawo zamówień publicznych"
      "(tekst jednolity Dz. U. z 2013 r. poz. 907) na niniejszy wyrok")
    {:journalYear "2013" :journalNo "0" :journalEntry "907"})
  (extract-law-journal-case-one
    (str "ustawy z dnia 29 stycznia 2004 r. - Prawo zamówień publicznych"
      " (Dz. U. z 2010 r. 113, poz. 759 ze zm.)")
    {:journalYear "2010" :journalNo "113" :journalEntry "759"})
  (extract-law-journal-case-one
    (str "Ustawy z dnia 16 września 2011 r. o zmianie ustawy"
      " o transporcie kolejowym [Dz.U.2011.230.1372], wszelkie wydane")
    {:journalYear "2011" :journalNo "230" :journalEntry "1372"})
  (extract-law-journal-case-one
    (str "ustawy z dnia 29 stycznia 2004 r. - Prawo zamówień publicznych"
      " (tekst jednolity Dz. U. z 2013 r., poz. 907) na niniejszy wyrok")
    {:journalYear "2013" :journalNo "0" :journalEntry "907"})
  (extract-law-journal-case-one
    (str "ustawy z dnia 15 grudnia 2000 r. o samorządach zawodowych"
      " architektów, inżynierów budownictwa oraz urbanistów Dz.U.01.5.42"
      " ze zm. od 8 lutego 2001r. okręgowe izby inżynierów budownictwa")
    {:journalYear "2001" :journalNo "5" :journalEntry "42"}))

(deftest handle-superscript-test []
  (is (=
       (#'saos-tm.extractor.law-links/handle-superscript "5051")
       "505(1)"))
  (is (=
       (#'saos-tm.extractor.law-links/handle-superscript "5051-5052")
       "505(1)-505(2)")))

(deftest extract-coords-test []
  (is (=
       (#'saos-tm.extractor.law-links/extract-coords " Art. 52 ust. 3")
       '("52" "0" "3" "0" "0" "0"))))

(deftest stems-match?-test
  (is (=
       (#'saos-tm.extractor.law-links/stems-match? ["a" "b" "c"] ["b" "d"])
       true))
  (is (=
       (#'saos-tm.extractor.law-links/stems-match? ["a" "c"] ["b" "d"])
       false)))

(deftest tokens-match?-test
  (is (=
       (#'saos-tm.extractor.law-links/tokens-match?
        ["prawo" "cywilne"] ["prawa" "cywilnego"])
       true))
  (is (=
       (#'saos-tm.extractor.law-links/tokens-match?
        ["prawo" "karne"] ["prawa" "cywilnego"])
       false)))

(deftest extract-year-journal-nmb-and-entry-test
  (is (=
       (#'saos-tm.extractor.law-links/extract-year-journal-nmb-and-entry
        (#'saos-tm.extractor.law-links/split-to-tokens
         (str "ustawy z dnia 21 sierpnia 1997 r. o ograniczeniu"
              " prowadzenia działalności gospodarczej przez osoby"
              " pełniące funkcje publiczne (jednolity tekst: "
              "Dz.U. z 2006 r. Nr 216, poz. 1584 ze zm.)")))
       {:journalEntry "1584", :journalNo "216", :journalYear "2006"})))

(deftest cut-to-first-parenthesis-pair-test
  (is (=
       (#'saos-tm.extractor.law-links/cut-to-first-parenthesis-pair
        (str "z dnia 29 stycznia 2004 r. – Prawo zamówień publicznych"
             " (t.j. Dz. U. z 2010 r. 113, poz. 759 ze zm.) na niniejszy wyrok"
             " – w terminie 7 dni od dnia jego doręczenia – "
             "przysługuje skarga za pośrednictwem Prezesa "
             "Krajowej Izby Odwoławczej"
             " do Sądu Okręgowego w Warszawie. Przewodniczący"))
       (str "z dnia 29 stycznia 2004 r. – Prawo zamówień publicznych"
            " (t.j. Dz. U. z 2010 r. 113, poz. 759 ze zm.)")))
  (is (=
       (#'saos-tm.extractor.law-links/cut-to-first-parenthesis-pair
        (str "z dnia 29 stycznia 2004 r. Prawo zamówień publicznych "
             "(t.j. Dz. U. z 2010 r. Nr 113, poz. 759 z późn. zm.), "
             "w trybie przetargu nieograniczonego. Ogłoszenie"))
       (str "z dnia 29 stycznia 2004 r. Prawo zamówień publicznych "
             "(t.j. Dz. U. z 2010 r. Nr 113, poz. 759 z późn. zm.)"))))

; Test of utilities for the art part of law link
; (sorting and string conversion)

(deftest convert-art-to-str-test
  (is (=
        (#'saos-tm.extractor.law-links/convert-art-to-str
          {:art "1" :par "2" :ust "3" :pkt "4" :lit "a" :zd "5" })
        "art. 1 § 2 ust. 3 pkt 4 lit. a zd. 5"))
  (is (=
        (#'saos-tm.extractor.law-links/convert-art-to-str
          {:art "1" :par "0" :ust "0" :pkt "3" :lit "a" :zd "4" })
        "art. 1 pkt 3 lit. a zd. 4"))
  (is (=
        (#'saos-tm.extractor.law-links/convert-art-to-str
          {:art "0" :par "0" :ust "0" :pkt "0" :lit "0" :zd "0" })
        "")))

(deftest chain-compare-number-letter-test
  (let [
        compare-f
        #'saos-tm.extractor.law-links/chain-compare-number-letter
        ]
    (is (< (compare-f 0 "1" "2") 0))
    (is (< (compare-f 0 "1" "11") 0))
    (is (< (compare-f 0 "1" "1a") 0))
    (is (< (compare-f 0 "1a" "1b") 0))
    (is (> (compare-f 0 "2" "1") 0))
    (is (> (compare-f 0 "11" "1") 0))
    (is (> (compare-f 0 "1c" "1") 0))
    (is (> (compare-f 0 "1c" "1b") 0))
    (is (= (compare-f 0 "11" "11") 0))
    (is (= (compare-f 0 "1c" "1c") 0))))

(deftest compare-art-sort-test
  (is (=
       (#'saos-tm.extractor.law-links/sort-arts
        [ {:art "3" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "2" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}])
       [ {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "2" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "3" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
       (#'saos-tm.extractor.law-links/sort-arts
        [ {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}])
       [ {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
       (#'saos-tm.extractor.law-links/sort-arts
        [ {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "12" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}])
       [ {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "12" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
       (#'saos-tm.extractor.law-links/sort-arts
        [ {:art "1" :par "4a" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}])
       [ {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "4a" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
       (#'saos-tm.extractor.law-links/sort-arts
        [ {:art "1" :par "3a" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
          {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}])
       [ {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3a" :ust "3" :pkt "4" :zd "5" :lit "a"}])))

(deftest conv-act-to-str-test
  (is (=
       (#'saos-tm.extractor.law-links/conv-act-to-str
        {:journalNo 23 :journalEntry 17 :journalYear 1996})
       "Dz. U. z 1996 r. Nr 23 poz. 17"))
  (is (=
       (#'saos-tm.extractor.law-links/conv-act-to-str
        {:journalEntry 1732 :journalYear 2015})
       "Dz. U. z 2015 r. poz. 1732")))

(deftest cleanse-commas-test
  (is (=
       (#'saos-tm.extractor.law-links/cleanse-commas
        "art , 24 ust 2 pkt 4")
       "art 24 ust 2 pkt 4"))
  (is (=
       (#'saos-tm.extractor.law-links/cleanse-commas
        "art 24 ust , 2 pkt 3 i 4")
       "art 24 ust 2 pkt 3 i 4")))

(deftest cast-coords-lists-test
  (is (=
       (#'saos-tm.extractor.law-links/cast-coords-lists
        '("1" "2" "0" "0" "0" "0") '("0" "3" "0" "0" "0" "0"))
       '("1" "3" "0" "0" "0" "0")))
  (is (=
       (#'saos-tm.extractor.law-links/cast-coords-lists
        '("1" "2" "0" "0" "2" "2") '("0" "3" "0" "0" "0" "0"))
       '("1" "3" "0" "0" "0" "0")))
  (is (=
       (#'saos-tm.extractor.law-links/cast-coords-lists
        '("1" "2" "0" "0" "2" "2") '("0" "3" "3" "0" "0" "0"))
       '("1" "3" "3" "0" "0" "0")))
  (is (=
       (#'saos-tm.extractor.law-links/cast-coords-lists
        '("1" "2" "0" "0" "2" "2") '("0" "0" "3" "0" "0" "0"))
       '("1" "2" "3" "0" "0" "0"))))

(deftest extract-art-coords-test
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        "art 90")
       '(("90" "0" "0" "0" "0" "0"))))
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        "art. 183 ust 5 pkt 2 oraz 6")
       '(("183" "0" "5" "2" "0" "0") ("183" "0" "5" "6" "0" "0"))))
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        "art. 183 ust 5 pkt 2 oraz ust. 6")
       '(("183" "0" "5" "2" "0" "0") ("183" "0" "6" "0" "0" "0"))))
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        "art. 89 ust. 1 pkt 2 , pkt 3 , pkt 8")
       '(("89" "0" "1" "2" "0" "0")
         ("89" "0" "1" "3" "0" "0")
         ("89" "0" "1" "8" "0" "0"))))
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        " art. 89 ust. 1 pkt 2 oraz pkt 4")
       '(("89" "0" "1" "2" "0" "0") ("89" "0" "1" "4" "0" "0"))))
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        "art. 24 ust. 1 pkt 10 oraz ust. 2 pkt 2")
       '(("24" "0" "1" "10" "0" "0") ("24" "0" "2" "2" "0" "0"))))
  (is (=
       (#'saos-tm.extractor.law-links/extract-art-coords
        "art. 89 ust. 1 pkt. 2 i pkt. 6")
       '(("89" "0" "1" "2" "0" "0") ("89" "0" "1" "6" "0" "0")))))

(deftest get-year-of-law-act-test
  (is (=
       "2007"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         " ustawy Prawo zamówień publicznych ( Dz. U. t.j. z 2007 r. Nr 223"
         " , poz. 1655 ). O kosztach postępowania orzeczono na podstawie"))))
  (is (=
       "2006"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         "rozporządzenia Prezesa Rady Ministrów z dnia 17 maja 2006"
         " w sprawie wysokości oraz szczegółowych zasad pobierania"
         " wpisu od odwołania oraz szczegółowych zasad rozliczania"
         " kosztów w postępowaniu odwoławczym ( Dz. U. Nr 87 , poz. 608 )"))))
  (is (=
       "1992"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         "KONSTYTUCYJNE utrzymane w mocy na podstawie art. 77"
         " Ustawy Konstytucyjnej"
         " z dnia 17 października 1992 r. o wzajemnych stosunkach między"
         " władzą ustawodawczą i wykonawczą Rzeczypospolitej Polskiej "
         "oraz o samorządzie terytorialnym "
         "(Dz. U. Nr 84, poz. 426, z 1995 r. Nr 38, poz. 184): "
         "(uchylony) ogólnie – w. 6.01.09, SK 22/06 (poz. 1), w. 15.01.09"))))
  (is (=
       "1994"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         " Karta Samorządu Lokalnego sporządzona w Strasburgu"
         " dnia 15 października 1985 r. (Dz. U. z 1994 r. Nr 124, poz. 607"
         " oraz z 2006 r. Nr 154, poz. 1107): art. 4 ust. 2 i 6 "
         "– p. 21.01.09, P 14/08 (poz. 7)"))))
  (is (=
       "1994"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         " ustawy z dnia 28 grudnia 1989 r. – Prawo celne"
         " (tekst jednolity z 1994 r. Dz.U. Nr 71, poz. 312 ze zm.)"))))
  (is (=
       "1991"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         "ustawy z dnia 30 sierpnia 1991 r. o zakładach opieki zdrowotnej"
         " (Dz.U. Nr 91, poz. 408 ze zm.) kjhkjh "
         "(Dz.U. z 2001 r. Nr 65, poz. 659)"))))
  (is (=
       "2007"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         "Prezesa Rady Ministrów z dnia 19 grudnia 2007r. w sprawie kwot "
         "wartości zamówień oraz konkursów, od których jest "
         "uzależniony obowiązek przekazywania ogłoszeń "
         "Urzędowi Oficjalnych Publikacji Wspólnot "
         "Europejskich (Dz. U. Nr 241,  poz.  1762)"))))
  (is (=
       "2006"
       (#'saos-tm.extractor.law-links/get-year-of-law-act
        (str
         "Prezesa Rady Ministrów z dnia 19.05.2006 r. w sprawie rodzajów "
         "dokumentów, jakich może żądać zamawiający od wykonawcy, oraz "
         "form w jakich te dokumenty mogą być składane "
         "(Dz. U. nr 87  poz.  605)"))))
  )
