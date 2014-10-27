(ns saos-tm.extractor.osp-extract-parties-test
  (:require
    [clojure.test :refer :all]
    [ clojure.string :as str ]
    [ clojure.set :refer :all ]
    [ langlab.core.parsers :refer :all ]
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
                 "      <xUnit xIsTitle='true' xType='part'>\n"
                 "        <xName>UZASADNIENIE</xName>\n"
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
          actual
            (split-osp-judgment-to-parts judgment)
          ]
          (is (= actual expected))))

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
      " firmie MMM "))
  )

(deftest cleanse-party-test []
  (is (=
      (cleanse-party
        " <xBx><xAnon>B. W.\n\n</xAnon> i  <xAnon>S. W. (1)</xAnon></xBx>")
      "B. W. i S. W. (1)"))
  )