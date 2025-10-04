(defpackage :csv-validator
  (:use :cl)
  (:export
   :main
   :csvline->vector
   :csvconfig
   :check-integer-string
   :check-float-string
   :check-number-string
   :check-number-in-range
   :check-date-parsable
   :check-tz-parsable
   :check-null
   :check-not-null
   :check-date-before-today
   :check-compare-two-dates
   :check-integer-in-range
   :validate-csv-string
   :validate-csv-file   
   :make-validation-suite
   :make-rule

   ;; metrics section export getters and setters
   :metrics-suite-name
   :metrics-nlines
   :metrics-npass
   :metrics-nfail
   :metrics-found-headers
   :metrics-missing-headers
   :metrics-results
   :rule-metrics-rule-name
   :rule-metrics-rule-label
   :rule-metrics-npass
   :rule-metrics-nfail
   :rule-metrics-indices
   :rule-metrics-values
   ))
