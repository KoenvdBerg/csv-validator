;; tests the validation_utils
(in-package :csv-validator-tests)

(def-suite testmain
  :description "test suite for validation utils")

;;(setf fiveam:*on-failure* :debug)


(in-suite testmain)

;; data type tests
(test test-check-integer-string
  ;; not valid
  (is (not (csv-validator:check-integer-string "-klsdf")))
  (is (not (csv-validator:check-integer-string "1.42")))
  (is (not (csv-validator:check-integer-string "24ksd42")))
  ;; valid
  (is (csv-validator:check-integer-string "-191"))
  (is (csv-validator:check-integer-string "38"))
  (is (csv-validator:check-integer-string "002")))

(test test-check-float-string
  ;; not valid
  (is (not (csv-validator:check-float-string "-klsdf")))
  (is (not (csv-validator:check-float-string "-191")))
  (is (not (csv-validator:check-float-string "24ksd42")))
  ;; valid
  (is (csv-validator:check-float-string "1.42"))
  (is (csv-validator:check-float-string "38.4"))
  (is (csv-validator:check-float-string "002.29")))


(test test-check-scientific-number-string
  ;; not valid
  (is (not (csv-validator:check-scientific-number-string "-klsdf")))
  (is (not (csv-validator:check-scientific-number-string "-191")))
  (is (not (csv-validator:check-scientific-number-string "24ksd42")))
  ;; valid
  (is (csv-validator:check-scientific-number-string "1.42E10"))
  (is (csv-validator:check-scientific-number-string "38.4e-3"))
  (is (csv-validator:check-scientific-number-string "002.29e9")))


(test test-check-number-string
  ;; not valid
  (is (not (csv-validator:check-number-string "-klsdf")))
  (is (not (csv-validator:check-number-string "24ksd42")))
  ;; valid
  (is (csv-validator:check-number-string "-191"))
  (is (csv-validator:check-number-string "1.42E10"))
  (is (csv-validator:check-number-string "38.4"))
  (is (csv-validator:check-number-string "002.29e9")))

(test test-check-date-parsable
  ;; not valid
  (is (not (csv-validator:check-date-parsable "klsdf")))
  (is (not (csv-validator:check-date-parsable "2800-27-01")))
  (is (not (csv-validator:check-date-parsable "2800-01-77")))
  (is (not (csv-validator:check-date-parsable "2800-1-77")))
  (is (not (csv-validator:check-date-parsable "2022/01/01")))
  ;; valid
  (is (csv-validator:check-date-parsable "0101-01-01"))
  (is (csv-validator:check-date-parsable "2022-01-01 00:00:00"))
  (is (csv-validator:check-date-parsable "3800-01-02")))


(test test-check-tz-parsable
  ;; not valid
  (is (not (csv-validator:check-tz-parsable "klsdf")))
  (is (not (csv-validator:check-tz-parsable "2800-27-01 00:00:00")))
  (is (not (csv-validator:check-tz-parsable "2800-01-77")))
  (is (not (csv-validator:check-tz-parsable "2800-1-77")))
  (is (not (csv-validator:check-tz-parsable "2022/01/01")))
  (is (not (csv-validator:check-tz-parsable "1993-02-30T30:09:20Z")))
  ;; valid
  (is (csv-validator:check-tz-parsable "2022-01-01T00:00:00Z"))
  (is (csv-validator:check-tz-parsable "1993-05-21T21:09:20Z")))


(test test-check-null
  ;; not valid
  (is (not (csv-validator:check-null "-klsdf")))
  (is (not (csv-validator:check-null "923")))
  (is (not (csv-validator:check-null "2020-06-12")))
  ;; valid
  (is (csv-validator:check-null "NA"))
  (is (csv-validator:check-null ""))
  (is (csv-validator:check-null "null"))
  (is (csv-validator:check-null "NaN")))


(test test-check-date-before-today
  ;; not valid
  (is (not (csv-validator:check-date-before-today "2099-01-01")))
  ;; valid
  (is (csv-validator:check-date-before-today "2022-02-02"))
  (is (csv-validator:check-date-before-today "2022-01-01"))
  (is (csv-validator:check-date-before-today "klsdf"))
  (is (csv-validator:check-date-before-today "2022-11-01"))
  (is (csv-validator:check-date-before-today "1997-07-28"))
  (is (csv-validator:check-date-before-today "2022-01-01")))

(test test-compare-two-dates
  ;; not valid
  (is (not (csv-validator:check-compare-two-dates "2022-02-02" "2022-03-03")))
  ;; valid
  (is (csv-validator:check-compare-two-dates "2022-02-02" "2022/03/03"))
  (is (csv-validator:check-compare-two-dates "2022-01-01" "kldsa"))
  (is (csv-validator:check-compare-two-dates "klsdf" "kldsa"))
  (is (csv-validator:check-compare-two-dates "2022-11-01" "2021-01-01"))
  (is (csv-validator:check-compare-two-dates "1997-07-28" "1995-09-09"))
  (is (csv-validator:check-compare-two-dates "2022-01-01" "2021-01-01")))


(test test-check-number-in-range
  ;; not valid
  (is (not (check-number-in-range "5" 6 10)))
  (is (not (check-number-in-range "abc" -10 0)))
  (is (not (check-number-in-range "5e3" 6 10)))
  (is (not (check-number-in-range "abc" -10 0)))
  ;; valid
  (is (check-number-in-range "5.3" 0 10))
  (is (check-number-in-range "-5e0" -10 0))
  (is (check-number-in-range "5" 0 10))
  (is (check-number-in-range "-5" -10 0)))

