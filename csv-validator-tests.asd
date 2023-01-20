(in-package :asdf-user)
(defsystem "csv-validator-tests"
  :description "Test suite for the csv-validator system"
  :author "Koen van den Berg <k.vandenberg@insertdata.nl>"
  :version "0.0.1"
  :depends-on (:csv-validator
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-csv-validator"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
