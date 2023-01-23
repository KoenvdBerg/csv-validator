(in-package :asdf-user)

(defsystem "csv-validator"
  :author "Koen van den Berg <k.vandenberg@insertdata.nl>"
  :version "0.0.1"
  :license "MIT"
  :description ""
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (:local-time :clingon :lparallel :parse-float)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "main")
				     (:file "parallel")
				     (:file "output")
				     (:file "csv")
				     (:file "validator")
     				     (:file "validation_utils")
				     (:file "validation_suites"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "csv-validator"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "csv-validator:main")
