(ns saos-tm.extractor.osp-extract-parties-test
  (:require
    [clojure.test :refer :all]
    [ clojure.string :as str ]
    [ clojure.set :refer :all ]
    [ langlab.core.parsers :refer :all ]
    [saos-tm.extractor.common :refer :all]
    [saos-tm.extractor.osp-extract-parties :refer :all])
  (:import java.io.File)
  (:gen-class))

(deftest extract-osp-judgments-test
  (let [
          xml (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                   "<judgements>\n"
                   "  <judgement id=\"0\">\n"
                   "    <xPart>\n"
                   "      <xName>Postanowienie+Uzasadnienie</xName>\n"
                   "    </xPart>\n"
                   "  </judgement>\n"
                   "  <judgement id=\"1\">\n"
                   "    <xPart>\n"
                   "      <xName>Postanowienie+Uzasadnienie</xName>\n"
                   "    </xPart>\n"
                   "  </judgement>\n"
                   "  <judgement id=\"2\">\n"
                   "    <xPart>\n"
                   "      <xName>Wyrok</xName>\n"
                   "    </xPart>\n"
                   "  </judgement>\n"
                   "</judgements>")
          expected
            [(str  "<judgement id=\"0\">\n"
                   "    <xPart>\n"
                   "      <xName>Postanowienie+Uzasadnienie</xName>\n"
                   "    </xPart>\n"
                   "  </judgement>")
             (str   "<judgement id=\"1\">\n"
                   "    <xPart>\n"
                   "      <xName>Postanowienie+Uzasadnienie</xName>\n"
                   "    </xPart>\n"
                   "  </judgement>")
             (str   "<judgement id=\"2\">\n"
                   "    <xPart>\n"
                   "      <xName>Wyrok</xName>\n"
                   "    </xPart>\n"
                   "  </judgement>")]
          actual  (extract-osp-judgments xml)
          ]
          (is (= actual expected))))

(deftest split-osp-judgment-to-parts-test []
  (let [
          judgment1
            (str "<?xml version='1.0' encoding='UTF-8'?>\n"
                 "<judgement id='0'>\n"
                 "  <xPart>\n"
                 "    <xName>Postanowienie+Uzasadnienie</xName>\n"
                 "    <xBlock>\n"
                 "      <xText>Sygn. akt III K 9/10</xText>\n"
                 "      <xUnit xIsTitle='true' xBold='true' xType='part'>\n"
                 "        <xName>POSTANOWIENIE</xName>\n"
                 "        <xUnit xIsTitle='false' xType='none'/>\n"
                 "      </xUnit>\n"
                 "      <xUnit xIsTitle='true' xType='part'>\n"
                 "        <xName>UZASADNIENIE</xName>\n"
                 "        <xUnit xIsTitle='false' xType='none'/>\n"
                 "      </xUnit>\n"
                 "    </xBlock>\n"
                 "  </xPart>\n"
                 "</judgement>")
          expected1
            [(str
              "<?xml version='1.0' encoding='UTF-8'?>\n"
              "<judgement id='0'>\n"
              "<xPart>\n"
              "<xName>Postanowienie+Uzasadnienie</xName>\n"
              "<xBlock>\n"
              "<xText>Sygn. akt III K 9/10</xText>\n"
              "<xUnit xIsTitle='true' xBold='true' xType='part'>\n"
              "<xName>POSTANOWIENIE</xName>\n"
              "<xUnit xIsTitle='false' xType='none'/>\n"
              "</xUnit>\n"
              "</xBlock>\n"
              "</xPart>\n"
              "</judgement>\n")
            (str
              "<?xml version='1.0' encoding='UTF-8'?>\n"
              "<judgement id='0'>\n"
              "<xPart>\n"
              "<xName>Postanowienie+Uzasadnienie</xName>\n"
              "<xBlock>\n"
              "<xUnit xIsTitle='true' xType='part'>\n"
              "<xName>UZASADNIENIE</xName>\n"
              "<xUnit xIsTitle='false' xType='none'/>\n"
              "</xUnit>\n"
              "</xBlock>\n"
              "</xPart>\n"
              "</judgement>\n")]
          actual1
            (split-osp-judgment-to-parts judgment1)
          judgment2
            (str "<?xml version='1.0' encoding='UTF-8'?>\n"
                 "<judgement id='0'>\n"
                 "  <xPart>\n"
                 "    <xName>Postanowienie+Uzasadnienie</xName>\n"
                 "    <xBlock>\n"
                 "      <xUnit xIsTitle='true' xBold='true' xType='part'>\n"
                 "        <xName>POSTANOWIENIE</xName>\n"
                 "        <xUnit xIsTitle='false' xType='none'/>\n"
                 "      </xUnit>\n"
                 "      <xUnit xIsTitle='true' xType='part'>\n"
                 "        <xName>UZASADNIENIE</xName>\n"
                 "        <xUnit xIsTitle='false' xType='none'/>\n"
                 "      </xUnit>\n"
                 "    </xBlock>\n"
                 "  </xPart>\n"
                 "</judgement>")
          expected2
            [(str
              "<?xml version='1.0' encoding='UTF-8'?>\n"
              "<judgement id='0'>\n"
              "<xPart>\n"
              "<xName>Postanowienie+Uzasadnienie</xName>\n"
              "<xBlock>\n"
              "<xUnit xIsTitle='true' xBold='true' xType='part'>\n"
              "<xName>POSTANOWIENIE</xName>\n"
              "<xUnit xIsTitle='false' xType='none'/>\n"
              "</xUnit>\n"
              "</xBlock>\n"
              "</xPart>\n"
              "</judgement>\n")
            (str
              "<?xml version='1.0' encoding='UTF-8'?>\n"
              "<judgement id='0'>\n"
              "<xPart>\n"
              "<xName>Postanowienie+Uzasadnienie</xName>\n"
              "<xBlock>\n"
              "<xUnit xIsTitle='true' xType='part'>\n"
              "<xName>UZASADNIENIE</xName>\n"
              "<xUnit xIsTitle='false' xType='none'/>\n"
              "</xUnit>\n"
              "</xBlock>\n"
              "</xPart>\n"
              "</judgement>\n")]
          actual2
            (split-osp-judgment-to-parts judgment2)
          ]
          (is (= actual1 expected1))
          (is (= actual2 expected2))))

(deftest split-osp-judgment-to-parts-only1part-test []
  (let [
          judgment
            (str "<?xml version='1.0' encoding='UTF-8'?>\n"
                 "<judgement id='0'>\n"
                 "  <xPart>\n"
                 "    <xName>Postanowienie+Uzasadnienie</xName>\n"
                 "    <xBlock>\n"
                 "      <xText>Sygn. akt III K 9/10</xText>\n"
                 "      <xUnit xIsTitle='true' xBold='true' xType='part'>\n"
                 "        <xName>POSTANOWIENIE</xName>\n"
                 "        <xUnit xIsTitle='false' xType='none'/>\n"
                 "      </xUnit>\n"
                 "    </xBlock>\n"
                 "  </xPart>\n"
                 "</judgement>")
          expected
            [(str
              "<?xml version='1.0' encoding='UTF-8'?>\n"
              "<judgement id='0'>\n"
              "<xPart>\n"
              "<xName>Postanowienie+Uzasadnienie</xName>\n"
              "<xBlock>\n"
              "<xText>Sygn. akt III K 9/10</xText>\n"
              "<xUnit xIsTitle='true' xBold='true' xType='part'>\n"
              "<xName>POSTANOWIENIE</xName>\n"
              "<xUnit xIsTitle='false' xType='none'/>\n"
              "</xUnit>\n"
              "</xBlock>\n"
              "</xPart>\n"
              "</judgement>\n")]
          actual
            (split-osp-judgment-to-parts judgment)
          ]
          (is (= actual expected))))

(deftest extract-defendant-test []
  (is (=
        (extract-defendant
          "przeciwko firmie MMM </xText> <xText> jsdkfhskdjf </xText>")
          "<xText>firmie MMM</xText>"))
  (is (=
        (extract-defendant
          (str " Prokuratora Tomasza Janeczka</xText><xText>po rozpoznaniu"
            " w dniu 13 listopada 2008</xText><xText>sprawy z"
            " wniosku</xText><xText><xBx><xAnon>W."
            " A.</xAnon></xBx>(<xBx><xAnon>A.</xAnon></xBx>), s."
            " <xAnon>W.</xAnon>i <xAnon>J.</xAnon>, <xAnon>ur. (...)</xAnon>w"
            " <xAnon>S.</xAnon></xText><xText>o odszkodowanie i"
            " zadośćuczynienie za doznaną krzywdę"))
          (str "<xText><xAnon>W."
            " A.</xAnon>(<xAnon>A.</xAnon>), s."
            " <xAnon>W.</xAnon>i <xAnon>J.</xAnon>, <xAnon>ur. (...)</xAnon>w"
            " <xAnon>S.</xAnon></xText>"))))

(deftest cleanse-party-test []
  (is (=
      (cleanse-party
        " <xBx><xAnon>B. W.\n\n</xAnon> i  <xAnon>S. W. (1)</xAnon></xBx>")
      "<xAnon>B. W.</xAnon> i <xAnon>S. W. (1)</xAnon>"))
  )

(deftest close-xtext-tags-test []
  (let [
          expected "<xText>sth</xText>"
    ]
  (is (= (close-xtext-tags "sth") expected))
  (is (= (close-xtext-tags "<xText>sth") expected))
  (is (= (close-xtext-tags "sth</xText>") expected))
  (is (= (close-xtext-tags "<xText>sth</xText>") expected))))

(deftest remove-xLexLink-tags-test []
  (is (=
      (remove-xLexLink-tags
        (str "<xText>na podstawie <xLexLink xArt='art. 46;art. 46 § 1'"
          " xIsapId='WDU19970880553' xTitle='Ustawa z dnia 6 czerwca 1997 r. -"
          " Kodeks karny' xAddress='Dz. U. z 1997 r. Nr 88, poz. 553'>art. 46 §"
          " 1 k.k.</xLexLink> orzeka"))
      "<xText>na podstawie  orzeka")))

(deftest remove-xTexts-test []
  (is (= (remove-xTexts "aaa<xText/>bbb") "aaabbb"))
  (is (= (remove-xTexts "aaa<xText xALIGNx=\"center\"/>bbb") "aaabbb")))


;; (def not-thrown? (complement nil?))

;; (deftest spit-parties-test []
;;   (is (not-thrown? Exception (spit-parties))))
