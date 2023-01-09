(in-package :asdf-user)

(defsystem "stream_validator"
  :author "Koen van den Berg <k.vandenberg@insertdata.nl>"
  :version "0.0.1"
  :license "MIT"
  :description ""
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (:local-time :cl-csv :clingon :trivial-shell :lparallel)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "stream_validator")
				     (:file "parallel")
				     (:file "validator")
				     (:file "taxi_suite"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "stream_validator"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "stream_validator:main")
