(ns saos-tm.extractor.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str ]
            [saos-tm.extractor.common :refer :all]
            [saos-tm.extractor.law-links :refer :all]
            [saos-tm.extractor.judgment-links :refer :all]
            [saos-tm.extractor.law-links-trainset :refer :all]
            [langlab.core.parsers :refer [ lg-split-tokens-bi ] ]))

(deftest article-coords-test
  (is(=
    [["4" "0" "0" "2" "0" "0"]]
    (extract-coords "art. 4 pkt 2")))
  (is(=
    [["14a" "0" "0" "0" "0" "0"]]
    (extract-coords "art. 14a ")))
  (is(=
    [["50" "0" "1" "1" "0" "0"]]
    (extract-coords "art. 50 ust. 1 pkt 1 ")))
  (is(=
    [["3" "0" "2-3" "0" "0" "0"]]
    (extract-coords "art. 3 ust. 2-3 ")))
  (is(=
    [["103-105" "0" "0" "0" "0" "0"]]
    (extract-coords "art. 103-105 ")))
  (is(=
    [["47" "0" "1" "2" "0" "0"] ["47" "0" "1" "3" "0" "0"]]
    (extract-coords "art. 47 ust. 1 pkt 2 i 3 ")))
  (is(=
    [["0" "44" "1" "1" "0" "0"]]
    (extract-coords "§ 44 ust. 1 pkt 1 ")))
  (is(=
    [["0" "25" "3" "0" "0" "0"]]
    (extract-coords "§ 25 ust. 3 ")))
  (is(=
    [["0" "68a" "1" "0" "0" "0"]]
    (extract-coords "§ 68a ust. 1 ")))
  (is(=
    [["0" "79" "0" "0" "0" "0"]]
    (extract-coords "§ 79 ")))
  (is(=
    [["0" "34" "3" "2" "0" "0"]]
    (extract-coords "§ 34 ust. 3 pkt 2 ")))
  (is(=
    [["37" "4a" "0" "0" "0" "0"] ["37" "4b" "0" "0" "0" "0"]]
    (extract-coords "art. 37 § 4a, 4b ")))
  (is(=
    [["56" "1-3" "0" "0" "0" "0"]]
    (extract-coords "art. 56 § 1-3 ")))
  (is(=
    [["77" "1" "0" "0" "0" "0"] ["77" "2" "0" "0" "0" "0"]
     ["77" "2a" "0" "0" "0" "0"] ["77" "3" "0" "0" "0" "0"]
     ["77" "3a" "0" "0" "0" "0"] ["77" "6" "0" "0" "0" "0"]
     ["77" "7a" "0" "0" "0" "0"] ["77" "7b" "0" "0" "0" "0"]]
    (extract-coords "art. 77 § 1, 2, 2a, 3, 3a, 6, 7a i 7b ")))
  (is(=
    [["46" "1" "0" "0" "1" "0"]]
    (extract-coords "art. 46 § 1 zd. 1 ")))
  (is(=
    [["178" "0" "1" "0" "0" "0"] ["91" "0" "1" "0" "0" "0"]]
    (extract-coords "art. 178 ust. 1 i art. 91 ust. 1")))
  (is(=
    [["84" "0" "0" "0" "0" "0"] ["92" "0" "1" "0" "0" "0"]
     ["31" "0" "3" "0" "0" "0"]]
    (extract-coords "art. 84, art. 92 ust. 1 i art. 31 ust. 3")))
  (is(=
    [["2" "0" "0" "0" "0" "0"] ["84" "0" "0" "0" "0" "0"]
     ["91" "0" "1" "0" "0" "0"] ["178" "0" "1" "0" "0" "0"]]
    (extract-coords "art. 2, art. 84, z art. 91 ust. 1, art. 178 ust. 1")))
  (is(=
    '(("64" "0" "2" "0" "0" "0") ("64" "0" "3" "0" "0" "0")
      ("84" "0" "0" "0" "0" "0"))
  	(extract-coords "art. 64 ust. 2 i 3 oraz art. 84")))
  (is(=
    '(("64" "0" "2" "0" "0" "a"))
    (extract-coords "art. 64 ust. 2 lit. a")))
  ; (is(= [[]] (extract-coords "")))
  )

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
    (get-line-with-signature (slurp "unit-tests-data/K_01_91.txt"))))
  (is (=
    "(K. 1/92)"
    (get-line-with-signature (slurp "unit-tests-data/K_01_92.txt"))))
  (is (=
    "Sygn. akt (K. 1 /93)"
    (get-line-with-signature (slurp "unit-tests-data/K_01_93.txt")))))

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

(deftest extract-signatures-institutions-test
  (is (= (extract-signatures-osp
        "IV CKN 178/01 II CSK 418/13 I 2 C 24/97")
      ["IV CKN 178/01" "II CSK 418/13" "I 2 C 24/97"]))
  (is (= (extract-signatures-osp
        (str "dnia 02.02.2006 r., sygn. akt: III Ca 727/05), to tym "
        "w wyroku z 15.06.2004 III K 38/03 - oddanie"
        "dnia 9 lutego 2007 r. III CZP 161/06 opubl. w OSNC 1/208 poz. 4"))
      ["III Ca 727/05" "III K 38/03" "III CZP 161/06"]))
  (is (= (extract-signatures-osp
        "I.2.C.24/97 II.CSK.418/13")
      ["I.2.C.24/97" "II.CSK.418/13"]))
  (is (= (extract-signatures-osp
        (str "marca 2005r. sygn. akt II K.350/04 obejmującego"
        "2004r. sygn. akt II K.184/04 obejmującego wyrok"
        "łącznym o sygn. akt II K 350/04 z dnia 22 marca 2005 r. "))
      ["II K.350/04" "II K.184/04" "II K 350/04"]))
  (is (= (extract-signatures-kio-no-space
        "KIO/UZP/1/07 KIO/UZP/102/08")
      ["KIO/UZP/1/07" "KIO/UZP/102/08"]))
  (is (= (extract-signatures-kio-no-space
        (str "  sygn. akt: KIO/UZP/42/07 oraz Wyrok KIO z dnia 22.02.2008 r.,"
        " sygn. akt: KIO/UZP/99/08) istnienie"))
      ["KIO/UZP/42/07" "KIO/UZP/99/08"]))
  (is (= (extract-signatures-kio-space
        "KIO/UZP 141/08 KIO/UZP 102/08")
      ["KIO/UZP 141/08" "KIO/UZP 102/08"]))
  (is (= (extract-signatures-kio-space
        (str "31.01.2008 r., sygn. akt: KIO/UZP 119/07 – przedłożony"
          "31.01.2008 r., sygn. akt: KIO/UZP\n119/08.\n"
          "Sygn. akt KIO/UZP 961/08\nWYROK\nz dnia 24 września 2008 r."))
      ["KIO/UZP 119/07" "KIO/UZP 119/08" "KIO/UZP 961/08"]))
  (is (= (extract-signatures-kio-uzp
        "UZP/ZO/0-1094/99, UZP/ZO/0-725/05")
      ["UZP/ZO/0-1094/99" "UZP/ZO/0-725/05"]))
  (is (= (extract-signatures-tk
        (str "1998 r., sygn. K 3/98, OTK ZU nr 4/1998, poz. 52; 23 lutego 1999"
          " r., sygn. K 25/98, OTK ZU nr 2/1999, poz. 23; 19 czerwca 2002 r."
          ", sygn. K 11/02, OTK ZU Sygn. akt K 10/09*"
          "CZP 161/06 opubl. w OSNC 1/208 poz. 4 "))
      ["K 3/98" "K 25/98" "K 11/02" "K 10/09"]))
  (is (= (extract-signatures-sn
        "BSA I-4110-4/13, BSA I-4110-5/07")
      ["BSA I-4110-4/13" "BSA I-4110-5/07"]))
  (is (= (extract-signatures-nsa
        "I SA/Bd 680/14, IV SA/Gl 543/14, I SA/Rz 794/11")
      ["I SA/Bd 680/14" "IV SA/Gl 543/14" "I SA/Rz 794/11"])))

(deftest extract-signatures-universal-test
  (is (= (extract-signatures-universal
        "15 października 2008 r. (sygn. Tw 19/08, OTK ZU nr 2/B/2009, poz. 67)")
      ["Tw 19/08"]))
  (is (= (extract-signatures-universal
        (str "się pod sygnaturą XI GCo 74/11, nie doprowadziło 15 października "
        "2008 r. (sygn. Tw 19/08, OTK ZU nr 2/B/2009, poz. 67)"))
      ["XI GCo 74/11", "Tw 19/08"]))
    )

(deftest extract-all-signatures-test
  (is (= (extract-all-signatures
        "października 2010 r. (sygn. akt I C-upr 14/10) oraz postanowieniem")
      ["I C-upr 14/10"]))
  (is (= (extract-all-signatures
        "października 2010 r. (sygn. akt 234 14/10) oraz postanowieniem")
      ["234 14/10"]))
  (is (= (extract-all-signatures
        (str "8 kwietnia 2009 r. (sygn.: Tw 9/08, Tw 11/08, Tw 12/08, Tw 16/08,"
        " Tw 19/08) Trybunał"))
      ["Tw 9/08","Tw 11/08","Tw 12/08","Tw 16/08","Tw 19/08"]))
  )