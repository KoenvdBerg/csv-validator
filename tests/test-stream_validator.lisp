;; tests the validation_utils
(in-package :stream_validator-tests)

(def-suite testmain
  :description "test suite for validation utils")

;;(setf fiveam:*on-failure* :debug)


(in-suite testmain)

;; data type tests
(test test-check-integer-string
  ;; not valid
  (is (not (stream_validator:check-integer-string "-klsdf")))
  (is (not (stream_validator:check-integer-string "1.42")))
  (is (not (stream_validator:check-integer-string "24ksd42")))
  ;; valid
  (is (stream_validator:check-integer-string "-191"))
  (is (stream_validator:check-integer-string "38"))
  (is (stream_validator:check-integer-string "002")))

(test test-check-date-parsable
  ;; not valid
  (is (not (stream_validator:check-date-parsable "klsdf")))
  (is (not (stream_validator:check-date-parsable "2800-27-01")))
  (is (not (stream_validator:check-date-parsable "2800-01-77")))
  (is (not (stream_validator:check-date-parsable "2800-1-77")))
  (is (not (stream_validator:check-date-parsable "2022/01/01")))
  ;; valid
  (is (stream_validator:check-date-parsable "0101-01-01"))
  (is (stream_validator:check-date-parsable "2022-01-01 00:00:00"))
  (is (stream_validator:check-date-parsable "3800-01-02")))



(test test-check-null
  ;; not valid
  (is (not (stream_validator:check-null "-klsdf")))
  (is (not (stream_validator:check-null "923")))
  (is (not (stream_validator:check-null "2020-06-12")))
  ;; valid
  (is (stream_validator:check-null "NA"))
  (is (stream_validator:check-null ""))
  (is (stream_validator:check-null "null"))
  (is (stream_validator:check-null "NaN")))

(test test-compare-two-dates
  ;; not valid
  (is (not (stream_validator:check-compare-two-dates "2022-02-02" "2022-03-03")))
      ;; valid
  (is (stream_validator:check-compare-two-dates "2022-02-02" "2022/03/03"))
  (is (stream_validator:check-compare-two-dates "2022-01-01" "kldsa"))
  (is (stream_validator:check-compare-two-dates "klsdf" "kldsa"))
  (is (stream_validator:check-compare-two-dates "2022-11-01" "2021-01-01"))
  (is (stream_validator:check-compare-two-dates "1997-07-28" "1995-09-09"))
  (is (stream_validator:check-compare-two-dates "2022-01-01" "2021-01-01")))


(test test-check-date-before-today
  ;; not valid
  (is (not (stream_validator:check-date-before-today "2099-01-01")))
  ;; valid
  (is (stream_validator:check-date-before-today "2022-02-02"))
  (is (stream_validator:check-date-before-today "2022-01-01"))
  (is (stream_validator:check-date-before-today "klsdf"))
  (is (stream_validator:check-date-before-today "2022-11-01"))
  (is (stream_validator:check-date-before-today "1997-07-28"))
  (is (stream_validator:check-date-before-today "2022-01-01")))


(test test-check-integer-in-range
  ;; not valid
  (is (not (check-integer-in-range "5" 6 10)))
  (is (not (check-integer-in-range "abc" -10 0)))
  (is (not (check-integer-in-range "5" 6 10)))
  (is (not (check-integer-in-range "abc" -10 0)))
  ;; valid
  (is (check-integer-in-range "5" 0 10))
  (is (check-integer-in-range "-5" -10 0))
  (is (check-integer-in-range "5" 0 10))
  (is (check-integer-in-range "-5" -10 0)))

