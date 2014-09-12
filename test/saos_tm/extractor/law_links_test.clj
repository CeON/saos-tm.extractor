(ns saos-tm.extractor.law-links-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str ]
            [saos-tm.extractor.common :refer :all]
            [saos-tm.extractor.law-links :refer :all]
            [langlab.core.parsers :refer [ lg-split-tokens-bi ] ]))

(deftest article-ranges-test []
  (let [
        tokens
          (lg-split-tokens-bi "pl"
            (str "jest niezgodny z art. 2, art. 31 ust. 3, art. 64 "
                 "ust. 1, 2 i 3, art. 84, art. 91 ust. 1, art. 92 ust. 1, "
                 "art. 178 ust. 1 Konstytucji i art. 13 pkt 1 i 3 aneksu A "
                 "„Ogólne warunki odnoszące się do memorandum finansowego” "
                 "do Umowy Ramowej"))]
  (is (=
    [[3 49][51 58]]
    (get-correct-art-coords-ranges tokens)))))

(deftest signature-extraction-test []
  (is (=
    "K. 1/00"
    (extract-signature "Sygn. K. 1/00 ")))
  (is (=
    "W. 17/92"
    (extract-signature "Sygn. akt (W. 17/92)")))
  (is (=
    "SK 22/06"
    (extract-signature "Sygn. akt SK 22/06*")))
  (is (=
    "Tw 51/12"
    (extract-signature "Sygn. akt Tw 51/12")))
  (is (=
    "W. 17/92"
    (extract-signature "Sygn. akt (W. 17/92)"))))

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
  '({:act {:pos "1656", :nr "237", :year "2008"},
    :art {:lit "0", :zd "0", :pkt "0", :ust "1-6", :par "0", :art "3"}}
    {:act {:pos "1656", :nr "237", :year "2008"},
    :art {:lit "0", :zd "0", :pkt "5", :ust "0", :par "0", :art "4"}}
    {:act {:pos "1656", :nr "237", :year "2008"},
    :art {:lit "0", :zd "0", :pkt "6", :ust "0", :par "0", :art "4"}}
    {:act {:pos "1656", :nr "237", :year "2008"},
    :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "57"}})
  (:extracted-links (extract-law-links
    (str "art. 3 ust. 1-6, art. 4 pkt 5 i 6, art. 57 ustawy z dnia 19 grudnia"
         " 2008 r. o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656)")
    "dictionary.txt"))))
  (is(=
    '({:act {:pos "1656", :nr "237", :year "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "4-6", :par "0", :art "3"}}
      {:act {:pos "1656", :nr "237", :year "2008"},
       :art {:lit "0", :zd "0", :pkt "6", :ust "0", :par "0", :art "4"}}
      {:act {:pos "483", :nr "78", :year "1997"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "2"}}
      {:act {:pos "483", :nr "78", :year "1997"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "1", :par "0", :art "32"}}))
  (:extracted-links (extract-law-links
    (str "art. 3 ust. 4-6 i art. 4 pkt 6 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656) "
         "z art. 2 i art. 32 ust. 1 Konstytucji")
    "dictionary.txt")))
  (is(=
    '({:act {:pos "1656", :nr "237", :year "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}
      {:act {:pos "1656", :nr "237", :year "2008"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "5"}}))
  (:extracted-links (extract-law-links
    (str "art. 3 w związku z art. 5 ustawy z dnia 19 grudnia 2008 r. "
         "o emeryturach pomostowych (Dz. U. Nr 237, poz. 1656)")
    "dictionary.txt")))
  (is(=
    '({:act {:nr "16" :pos "93", :year "1964"},
       :art {:lit "0", :zd "0", :pkt "0", :ust "0", :par "0", :art "3"}}))
  (:extracted-links (extract-law-links
    (str "art. 3 kc według ustawy o trybunale oprócz kodeksu wykroczeń ")
    "dictionary.txt")))
  )

(deftest tokens-to-string-test []
  (let [
          s (str "1) art. 9 ustawy z dnia 27 lipca 2001 r. – "
          "Prawo o ustroju sądów powszechnych (Dz. U. Nr 98, poz. 1070, ze zm.)"
          " z art. 2, art. 10, art. 45 ust. 1, art. 173, art. 176 ust. 2"
          " i art. 178 ust. 1 Konstytucji Rzeczypospolitej Polskiej;")
    ]
  (= (tokens-to-string (split-to-tokens s)) s)))
 
(deftest get-year-of-law-act-test
  (is (=
    "1992"
    (get-year-of-law-act
      (str
        "KONSTYTUCYJNE utrzymane w mocy na podstawie art. 77 Ustawy Konstytucyjnej"
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
        "– p. 21.01.09, P 14/08 (poz. 7)")))))
    
