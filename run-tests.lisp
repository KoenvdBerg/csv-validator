
(load "stream_validator.asd")
(load "stream_validator-tests.asd")

(ql:quickload "stream_validator-tests")

(in-package :stream_validator-tests)

(uiop:quit (if (run-all-tests) 0 1))
