(ns saos-tm.extractor.law-links-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str ]
            [clojure.java.io :as io]
            [clojure-csv.core :refer :all ]
            [saos-tm.extractor.common :refer :all]
            [saos-tm.extractor.law-links :refer :all]
            [saos-tm.extractor.common-test :refer :all]
            [langlab.core.parsers :refer [lg-split-tokens-bi]]))

(def ^:private act-dictionary
  (load-dictionary (io/resource "act_dictionary.txt")))

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
    act-dictionary))))
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
    act-dictionary)))
  (is(=
    '({:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}
      {:act {:journalEntry "1656", :journalNo "237", :journalYear "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "5"}}))
  (:extracted-links (extract-law-links-greedy
    (str "art. 3 w związku z art. 5 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656)")
    act-dictionary)))
  (is(=
    '({:act {:journalNo "16" :journalEntry "93", :journalYear "1964"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}))
  (:extracted-links (extract-law-links-greedy
    (str "art. 3 kc według ustawy o trybunale oprócz kodeksu wykroczeń ")
    act-dictionary)))
  )

(deftest tokens-to-string-test []
  (let [
          s (str "1) art. 9 ustawy z dnia 27 lipca 2001 r. – "
          "Prawo o ustroju sądów powszechnych "
          "(Dz. U. Nr 98, poz. 1070, ze zm.)"
          " z art. 2, art. 10, art. 45 ust. 1, art. 173, art. 176 ust. 2"
          " i art. 178 ust. 1 Konstytucji Rzeczypospolitej Polskiej;")
    ]
  (= (tokens-to-string (split-to-tokens s)) s)))

(deftest get-year-of-law-act-test []
  (is (=
    "1992"
    (get-year-of-law-act
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
    (get-year-of-law-act
      (str
        " Karta Samorządu Lokalnego sporządzona w Strasburgu"
        " dnia 15 października 1985 r. (Dz. U. z 1994 r. Nr 124, poz. 607"
        " oraz z 2006 r. Nr 154, poz. 1107): art. 4 ust. 2 i 6 "
        "– p. 21.01.09, P 14/08 (poz. 7)"))))
  (is (=
    "1994"
    (get-year-of-law-act
      (str
        " ustawy z dnia 28 grudnia 1989 r. – Prawo celne"
        " (tekst jednolity z 1994 r. Dz.U. Nr 71, poz. 312 ze zm.)")))))

(defn extract-law-journal-case-one [s answer]
  (is (=
    (extract-act-coords-greedy
      (split-to-tokens s)
      act-dictionary)
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
  (is (= (extract-coords " Art. 52 ust. 3") '(("52" "0" "3" "0" "0" "0")))))

(deftest extract-law-links-strict-test []
  (is (=
       (set
       (:extracted-links
        (extract-law-links-strict
         "art. 8, 45, 91 ust. 1 Konstytucji"
         act-dictionary)))
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
         "art. 2 § 2, art. 4, 5 § 2, art. 92 i 410 oraz art. 7 k.p.k."
         act-dictionary)))
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
