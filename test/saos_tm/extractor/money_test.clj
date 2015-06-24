(ns saos-tm.extractor.money-test
  (:require
    [clojure.test :refer :all]
    [saos-tm.extractor.money :refer :all]))

(deftest extract-money-ref-basic-test
  (is (=
        (extract-money "")
        []))
  (is (=
        (extract-money "Tu nic nie ma o pieniądzach.")
        []))
  (is (=
        (extract-money "To jest zł, ale nie ma kwoty.")
        []))
  (is (=
        (extract-money "To kosztowało 76 zł.")
        [{ :amount 76M :text "76 zł"}]))
  (is (=
        (extract-money
          "Zasądzono 23 mln zł kary, mimo iż straty wyniosły 50 zł.")
        [{ :amount 23000000M :text "23 mln zł"}
         { :amount 50M :text "50 zł"} ]))
  (is (=
        (extract-money
          "Czy Zasądzono 23 mln zł; mimo iż straty wyniosły 50 zł?")
        [{ :amount 23000000M :text "23 mln zł"}
         { :amount 50M :text "50 zł"} ]))

  ;; Negative money sums are not parsed
  (is (=
        (extract-money "Kwota -357 zł.")
        []))

  ;; Amounts that have ill-formed numbers are parsed to "ERROR".
  ;; This allows for inspection of typical errors encountered in text
  ;; and eventual correction
  (is (=
        (extract-money "Kwota 3.570.00 zł.")
        [{:amount "ERROR" :text "3.570.00 zł"}]))
  (is (=
        (extract-money "0,5 tys. zł")
        [{:amount 500M :text "0,5 tys. zł"}]))

  ;; US convention for decimal/decade separator is not supported
  (is (=
        (extract-money "0.5 tys. zł")
        [{:amount "ERROR" :text "0.5 tys. zł"}]))

  (is (=
        (extract-money "Zasądzono wysokie odszkodowanie 23 000 tys. zł.")
        [{ :amount 23000000M :text "23 000 tys. zł"} ])))

(deftest extract-money-refs-breaks-test

  ;; We allow for strange linebreaks before and after currency
  (is (=
        (extract-money
          "Zasądzono odszkodowanie 23 000 \ntys. zł.")
        [{ :amount 23000000M :text "23 000 \ntys. zł"}]))
  (is (=
        (extract-money
          "Zasądzono odszkodowanie 23 000 \ntys.\n zł.")
        [{ :amount 23000000M :text "23 000 \ntys.\n zł"}]))
  (is (=
        (extract-money
          "Zasądzono odszkodowanie 23 000 \ntys.\n zł\n 27 \n gr.")
        [{ :amount 23000000.27M :text "23 000 \ntys.\n zł\n 27 \n gr"}]))
  (is (=
         (extract-money
           "Zasądzono odszkodowanie 23 000 \ntys.\n zł,\n najwyższe w Polsce.")
         [{ :amount 23000000.00M :text "23 000 \ntys.\n zł"}]))
  ;; Linebreaks in numbers are not allowed.
  ;; Linebreak is treated as "hard" separator.
  (is (=
        (extract-money
        "Zasądzono odszkodowanie 23\n100 \ntys.\n zł\n 27 \n gr.")
        [{ :amount 100000.27M :text "100 \ntys.\n zł\n 27 \n gr"}])))

(deftest extract-money-refs-parentheses-test
  ;; Parenthesis check
  (is (=
        (extract-money
          "Zasądzona kwota jest wysoka (2 tys. zł)")
        [{ :amount 2000M :text "2 tys. zł"}]))
  (is (=
        (extract-money
          "Zasądzona kwota jest wysoka ( 2 tys. zł )")
        [{ :amount 2000M :text "2 tys. zł"}]))
  (is (=
        (extract-money
          "Zasądzona kwota jest wysoka 2 000 ( zł )")
        [])))


(deftest extract-max-ref-money-basic-test
  (is (=
        (extract-max-money "")
        nil))
  (is (=
        (extract-max-money "Tu nic nie ma o pieniądzach.")
        nil))
  (is (=
        (extract-max-money "To jest zł, ale nie ma kwoty.")
        nil))
  (is (=
        (extract-max-money "To kosztowało 76 zł.")
        { :amount 76M :text "76 zł"}))
  (is (=
        (extract-max-money
           "Kilka kwot --- 760 zł 30 gr, 12 zł, 0.5 tys. zł.")
        { :amount 760.30M :text "760 zł 30 gr"}))
  (is (=
        (extract-max-money
          "Kilka kwot --- 12 zł, 760 zł 30 gr, 0.5 tys. zł.")
        { :amount 760.30M :text "760 zł 30 gr"}))
  (is (=
        (extract-max-money
          "Kilka kwot --- 12 zł, 0.5 tys. zł, 760 zł 30 gr.")
        { :amount 760.30M :text "760 zł 30 gr"}))

   ;; In the case of a few equal sums the first occurence is returned
  (is (=
        (extract-max-money
          "Te dwie kwoty są równowazne- 3.000 zł. i 3 tys. zł.")
        { :amount 3000M :text "3.000 zł"}))
  (is (=
        (extract-max-money
          "Te dwie kwoty są równowazne- 3 tys. zł. i 3.000 zł.")
        { :amount 3000M :text "3 tys. zł"})))
