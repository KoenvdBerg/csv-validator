(in-package :asdf-user)

(defsystem "csv-validator"
  :author "Koen van den Berg <k.vandenberg@insertdata.nl>"
  :version "1.0.0"
  :license "BSD-3"
  :description "Validates tabular CSV data using predefined validations, similar to its Python counterpart 'Great Expectations'."
  :homepage ""
  :bug-tracker ""
  :source-control (:git "https://github.com/KoenvdBerg/csv-validator")

  ;; Dependencies.
  :depends-on (:local-time :lparallel :parse-float)

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
     				     (:file "validation_utils"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "csv-validator"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "csv-validator:validate-csv")
