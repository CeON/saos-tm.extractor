(ns saos-tm.extractor.law-links-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str ]
            [clojure.java.io :as io]
            [clojure-csv.core :refer :all ]
            [saos-tm.extractor.common :refer :all]
            [saos-tm.extractor.law-links :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [langlab.core.parsers :refer [lg-split-tokens-bi]]))

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
              (get-correct-art-coords-ranges tokens)))))

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
  (:extracted-links (extract-law-links-greedy
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
  (:extracted-links (extract-law-links-greedy
    (str "art. 3 ust. 4-6 i art. 4 pkt 6 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656) "
         "z art. 2 i art. 32 ust. 1 Konstytucji")
                     true true true)))
  (is(=
    '({:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}
      {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "5"}}))
  (:extracted-links (extract-law-links-greedy
    (str "art. 3 w związku z art. 5 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656)")
                     true true true)))
  (is(=
    '({:act {:journalNo "16" :journalEntry "93", :journalYear "1964"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}))
  (:extracted-links (extract-law-links-greedy
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
  (= (tokens-to-string (split-to-tokens s)) s)))

(defn extract-law-journal-case-one [s answer]
  (is (=
    (extract-act-coords-greedy
     (split-to-tokens s) [] [] dictionary-for-acts-strict)
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
       (handle-superscript "5051")
       "505(1)"))
  (is (=
       (handle-superscript "5051-5052")
       "505(1)-505(2)")))

(deftest extract-coords-test []
  (is (= (extract-coords " Art. 52 ust. 3") '("52" "0" "3" "0" "0" "0"))))

(deftest extract-law-links-strict-test []
  (is (=
       (set
       (:extracted-links
        (extract-law-links-strict
         "art. 8, 45, 91 ust. 1 Konstytucji")))
       #{{:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "8"},
         :act
         {:journalEntry "483", :journalNo "78", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "45"},
         :act
         {:journalEntry "483", :journalNo "78", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "1", :par "0", :art "91"},
         :act
         {:journalEntry "483", :journalNo "78", :journalYear "1997"}}}))
  (is (=
       (set
        (:extracted-links
        (extract-law-links-strict
         "art. 2 § 2, art. 4, 5 § 2, art. 92 i 410 oraz art. 7 k.p.k.")))
       #{{:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "2", :art "2"},
         :act
         {:journalEntry "555", :journalNo "89", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "4"},
         :act
         {:journalEntry "555", :journalNo "89", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "2", :art "5"},
         :act
         {:journalEntry "555", :journalNo "89", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "92"},
         :act
         {:journalEntry "555", :journalNo "89", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "410"},
         :act
         {:journalEntry "555", :journalNo "89", :journalYear "1997"}}
        {:art
         {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "7"},
         :act
         {:journalEntry "555", :journalNo "89", :journalYear "1997"}}})))

(deftest stems-match?-test
  (is (= (stems-match? ["a" "b" "c"] ["b" "d"]) true))
  (is (= (stems-match? ["a" "c"] ["b" "d"]) false)))

(deftest tokens-match?-test
  (is (= (tokens-match? ["prawo" "cywilne"] ["prawa" "cywilnego"]) true))
  (is (= (tokens-match? ["prawo" "karne"] ["prawa" "cywilnego"]) false)))

(deftest extract-year-journal-nmb-and-entry-test
  (is (= (extract-year-journal-nmb-and-entry
          (split-to-tokens
           (str "ustawy z dnia 21 sierpnia 1997 r. o ograniczeniu"
            " prowadzenia działalności gospodarczej przez osoby"
            " pełniące funkcje publiczne (jednolity tekst: "
            "Dz.U. z 2006 r. Nr 216, poz. 1584 ze zm.)")))
         {:journalEntry "1584", :journalNo "216", :journalYear "2006"})))

(deftest cut-to-first-parenthesis-pair-test
  (is (=
       (cut-to-first-parenthesis-pair
        (str "z dnia 29 stycznia 2004 r. – Prawo zamówień publicznych"
             " (t.j. Dz. U. z 2010 r. 113, poz. 759 ze zm.) na niniejszy wyrok"
             " – w terminie 7 dni od dnia jego doręczenia – "
             "przysługuje skarga za pośrednictwem Prezesa "
             "Krajowej Izby Odwoławczej"
             " do Sądu Okręgowego w Warszawie. Przewodniczący"))
       (str "z dnia 29 stycznia 2004 r. – Prawo zamówień publicznych"
            " (t.j. Dz. U. z 2010 r. 113, poz. 759 ze zm.)")))
  (is (=
       (cut-to-first-parenthesis-pair
        (str "z dnia 29 stycznia 2004 r. Prawo zamówień publicznych "
             "(t.j. Dz. U. z 2010 r. Nr 113, poz. 759 z późn. zm.), "
             "w trybie przetargu nieograniczonego. Ogłoszenie"))
       (str "z dnia 29 stycznia 2004 r. Prawo zamówień publicznych "
             "(t.j. Dz. U. z 2010 r. Nr 113, poz. 759 z późn. zm.)"))))

; Test of utilities for the art part of law link
; (sorting and string conversion)

(deftest convert-art-to-str-test
  (is (=
        (convert-art-to-str
          {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"})
        "art. 1 § 2 ust. 3 pkt 4 zd. 5 lit. a"))
  (is (=
        (convert-art-to-str
          {:art "1" :par "0" :ust "0" :pkt "3" :zd "4" :lit "a"})
        "art. 1 pkt 3 zd. 4 lit. a"))
  (is (=
        (convert-art-to-str
          {:art "0" :par "0" :ust "0" :pkt "0" :zd "0" :lit "0"})
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
        (sort-arts
          [ {:art "3" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "2" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}])
        [ {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "2" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "3" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
        (sort-arts
          [ {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}])
        [ {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
        (sort-arts
          [ {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "12" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}])
        [ {:art "1" :par "2" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "12" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
        (sort-arts
          [ {:art "1" :par "4a" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}])
        [ {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "4a" :ust "3" :pkt "4" :zd "5" :lit "a"}]))
  (is (=
        (sort-arts
          [ {:art "1" :par "3a" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
           {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}])
        [ {:art "1" :par "1" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3" :ust "3" :pkt "4" :zd "5" :lit "a"}
         {:art "1" :par "3a" :ust "3" :pkt "4" :zd "5" :lit "a"}])))

(deftest conv-act-to-str-test
  (is (=
       (conv-act-to-str {:journalNo 23 :journalEntry 17 :journalYear 1996})
       "Dz. U. z 1996 r. Nr 23 poz. 17"))
  (is (=
       (conv-act-to-str {:journalEntry 1732 :journalYear 2015})
       "Dz. U. z 2015 r. poz. 1732")))
