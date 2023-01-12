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
  :depends-on (:local-time :clingon :trivial-shell :lparallel)

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
				     (:file "suites"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "stream_validator"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "stream_validator:main")
