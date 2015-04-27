(ns saos-tm.extractor.ref-money-test
  (:require
    [clojure.test :refer :all]
    [saos-tm.extractor.ref-money :refer :all]))

(deftest extract-ref-money-test
  (is (=
        (extract-money-refs "")
        []))
  (is (=
        (extract-money-refs "Tu nic nie ma o pieniądzach.")
        []))
  (is (=
        (extract-money-refs "To jest zł, ale nie ma kwoty.")
        []))
  (is (=
        (extract-money-refs "To kosztowało 76 zł.")
        [{ :amount 76M :text "76 zł."}]))
  (is (=
        (extract-money-refs "Zasądzono wysokie odszkodowanie 23 mln zł, mimo iż straty nie były wielkie, ok. 50 zł.")
        [{ :amount 23000000M :text "23 mln zł,"} { :amount 50M :text "50 zł."} ]))
  (is (=
        (extract-money-refs "Kwota -357 zł.")
        []))
  (is (=
        (extract-money-refs "Zasądzono wysokie odszkodowanie 23 000 tys. zł.")
        [{ :amount 23000000M :text "23 000 tys. zł."} ]))
  (is (=
        (extract-money-refs "Zasądzono wysokie odszkodowanie 23 000 \ntys. zł.")
        [])))
