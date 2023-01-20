
(load "csv-validator.asd")
(load "csv-validator-tests.asd")

(ql:quickload "csv-validator-tests")

(in-package :csv-validator-tests)

(uiop:quit (if (run-all-tests) 0 1))
