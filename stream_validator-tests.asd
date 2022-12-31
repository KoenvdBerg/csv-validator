(in-package :asdf-user)
(defsystem "stream_validator-tests"
  :description "Test suite for the stream_validator system"
  :author "Koen van den Berg <k.vandenberg@insertdata.nl>"
  :version "0.0.1"
  :depends-on (:stream_validator
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-stream_validator"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
