(ns saos-tm.extractor.judgment-links-test
  (:require
    [clojure.test :refer :all]
    [saos-tm.extractor.judgment-links :as judgment-links]
    [langlab.core.parsers :refer [ lg-split-tokens-bi ]]))

(deftest extract-signatures-institutions-test
  (is (= (judgment-links/extract-signatures-osp
        "IV CKN 178/01 II CSK 418/13 I 2 C 24/97")
      #{"IV CKN 178/01" "II CSK 418/13" "I 2 C 24/97"}))
  (is (= (judgment-links/extract-signatures-osp
        (str "dnia 02.02.2006 r., sygn. akt: III Ca 727/05), to tym "
        "w wyroku z 15.06.2004 III K 38/03 - oddanie"
        "dnia 9 lutego 2007 r. III CZP 161/06 opubl. w OSNC 1/208 poz. 4"))
      #{"III Ca 727/05" "III K 38/03" "III CZP 161/06"}))
  (is (= (judgment-links/extract-signatures-osp
        "I.2.C.24/97 II.CSK.418/13")
      #{"I.2.C.24/97" "II.CSK.418/13"}))
  (is (= (judgment-links/extract-signatures-osp
        (str "marca 2005r. sygn. akt II K.350/04 obejmującego"
        "2004r. sygn. akt II K.184/04 obejmującego wyrok"
        "łącznym o sygn. akt II K 350/04 z dnia 22 marca 2005 r. "))
      #{"II K.350/04" "II K.184/04" "II K 350/04"}))
  (is (= (judgment-links/extract-signatures-kio-uzp-zo-no-space
        "KIO/UZP/1/07 KIO/UZP/102/08")
      #{"KIO/UZP/1/07" "KIO/UZP/102/08"}))
  (is (= (judgment-links/extract-signatures-kio-uzp-zo-no-space
        (str "  sygn. akt: KIO/UZP/42/07 oraz Wyrok KIO z dnia 22.02.2008 r.,"
        " sygn. akt: KIO/UZP/99/08) istnienie"))
      #{"KIO/UZP/42/07" "KIO/UZP/99/08"}))
  (is (= (judgment-links/extract-signatures-kio-uzp
        "KIO/UZP 141/08 KIO/UZP 102/08")
      #{"KIO/UZP 141/08" "KIO/UZP 102/08"}))
  (is (= (judgment-links/extract-signatures-kio-uzp
        (str "31.01.2008 r., sygn. akt: KIO/UZP 119/07 – przedłożony"
          "31.01.2008 r., sygn. akt: KIO/UZP\n119/08.\n"
          "Sygn. akt KIO/UZP 961/08\nWYROK\nz dnia 24 września 2008 r."))
      #{"KIO/UZP 119/07" "KIO/UZP 119/08" "KIO/UZP 961/08"}))
  (is (= (judgment-links/extract-signatures-kio-uzp-zo
        "UZP/ZO/0-1094/99, UZP/ZO/0-725/05")
      #{"UZP/ZO/0-1094/99" "UZP/ZO/0-725/05"}))
  (is (= (judgment-links/extract-signatures-nsa
        "I SA/Bd 680/14, IV SA/Gl 543/14, I SA/Rz 794/11")
      #{"I SA/Bd 680/14" "IV SA/Gl 543/14" "I SA/Rz 794/11"})))

(deftest extract-all-signatures-test
  (is (= (judgment-links/extract-all-signatures
        "października 2010 r. (sygn. akt I C-upr 14/10) oraz postanowieniem")
      #{"I C-upr 14/10"}))
  (is (= (judgment-links/extract-all-signatures
        "października 2010 r. (sygn. akt 234 14/10) oraz postanowieniem")
      #{"234 14/10"}))
  (is (= (judgment-links/extract-all-signatures
        (str "8 kwietnia 2009 r. (sygn.: Tw 9/08, Tw 11/08, Tw 12/08, Tw 16/08,"
        " Tw 19/08) Trybunał"))
      #{"Tw 9/08","Tw 11/08","Tw 12/08","Tw 16/08","Tw 19/08"}))
  (is (= (judgment-links/extract-all-signatures
        (str "podanej wyżej sygnaturze akt w celu rozpoznania, czy w tej"
          " sprawie nie zachodzą przesłanki Tw 19/08 do wyłączenia"))
      #{"Tw 19/08"}))
  (is (= (judgment-links/extract-all-signatures
        "15 października 2008 r. (sygn. Tw 19/08, OTK ZU nr 2/B/2009, poz. 67)")
      #{"Tw 19/08"}))
  (is (= (judgment-links/extract-all-signatures
        (str "się pod sygnaturą XI GCo 74/11, nie doprowadziło 15 października "
        "2008 r. (sygn. Tw 19/08, OTK ZU nr 2/B/2009, poz. 67)"))
      #{"XI GCo 74/11", "Tw 19/08"}))
    (is (= (judgment-links/extract-all-signatures
        (str "lkjldfkj Tw 19/08 sdkfjhsdfnr Tw 19/08"))
      #{"Tw 19/08"}))
  (is (= (judgment-links/extract-all-signatures
        (str "Sygn. akt 1990/1991"))
      #{}))
  (is (= (judgment-links/extract-all-signatures
        (str "1990/1991"))
      #{})))
