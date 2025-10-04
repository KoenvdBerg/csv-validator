;; tests the validation_utils
(in-package :csv-validator-tests)

(def-suite csv-validator
  :description "test the methods from the csv-validator")


;;----------------- VALIDATION UTIL TESTS -----------------
(def-suite* validation-utilities
  :description "test suite for validation utils"
  :in csv-validator)

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


;;----------------- CSV PARSER TESTS -----------------

(def-suite* csv-parser
  :description "test suite for parsing csv-lines"
  :in csv-validator)

(test test-check-csvline-to-vector
  (let ((config (csv-validator:csvconfig #\, #\" #\\)))

    ;; some successful tests
    (is (equalp (csv-validator:csvline->vector "1,2,3" 5 config) #("1" "2" "3")))
    (is (equalp (csv-validator:csvline->vector "\"1\",\"2\",\"3\"" 5 config) #("1" "2" "3")))

    ;; escaped delimiters are ignored and content is left intact
    (is (equalp (csv-validator:csvline->vector "alpha,beta,gam\\,ma" 5 config) #("alpha" "beta" "gam\\,ma")))

    ;; having a column size of less than total columns cuts off the column, as expected
    (is (equalp (csv-validator:csvline->vector "\"1\",\"2\",\"3\"" 2 config) #("1" "2")))

    ;; having the incorrect delimiter just parses the line
    (is (equalp (csv-validator:csvline->vector "\"1\";\"2\";\"3\"" 5 config) #("1\";\"2\";\"3")))

    ;; empty lines are parsed correctly as well
    (is (equalp (csv-validator:csvline->vector ",1,2,3," 5 config) #("" "1" "2" "3" "")))
    (is (equalp (csv-validator:csvline->vector ",,," 5 config) #("" "" "" "")))))


;;----------------- CSV VALIDATOR TESTS -----------------
(def-suite* csv-validator-main
  :description "test running the csv-validator main method"
  :in csv-validator)

(defparameter *test-suite*
  (make-validation-suite
   :name "test"
   :expected-n-columns 3
   :csv-config (csvconfig #\; #\" #\\)
   :validation-rules
   (list
    (make-rule
     :name "ID_rule"
     :column "ID"
     :depends (list "ID")
     :label "integer"
     :logic (symbol-function 'csv-validator:check-integer-string))
    (make-rule
     :name "technology_rule"
     :column "technology"
     :depends (list "technology")
     :label "string-length"
     :logic (lambda (x) (< (length x) 5)))
    (make-rule
     :name "source_rule"
     :column "source"
     :depends (list "source")
     :label "not-null"
     :logic (symbol-function 'csv-validator:check-not-null)))))


(defparameter *test-csv-input*
"id;technology;source
1;t1;BHM
2;t2;BHM
3;technology_too_long;BHM
4;t4;")


;; test main parameters of the suite
(test test-csv-validator-string
  (let ((result (validate-csv-string *test-csv-input* *test-suite* :mode 'summary)))
    (is (equal (metrics-suite-name result) "test"))
    (is (equal (metrics-nlines result) 4))
    (is (equal (metrics-npass result) 2))
    (is (equal (metrics-nfail result) 2))
    (is (equal (metrics-found-headers result) '("technology" "source")))
    (is (equal (metrics-missing-headers result) '("ID")))))

(test test-csv-validator-rules
  (let* ((result (validate-csv-string *test-csv-input* *test-suite* :mode 'complete))
	 (techresult (gethash "technology_rule" (metrics-results result))))
    (is (equal (rule-metrics-rule-name techresult) "technology_rule"))
    (is (equal (rule-metrics-rule-label techresult) "string-length"))
    (is (equal (rule-metrics-npass techresult) 3))
    (is (equal (rule-metrics-nfail techresult) 1))
    (is (equalp (rule-metrics-indices techresult) #(2)))
    (is (equalp (rule-metrics-values techresult) #(("technology_too_long"))))))

  
