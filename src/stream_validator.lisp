(in-package :stream_validator)

;; Define your project functionality here...
(defun validate/options ()
  "Returns the options for the `validate' command"
  (list
   (clingon:make-option
    :string
    :description "the csv infile to validate"
    :short-name #\i
    :long-name "infile"
    :required t
    :env-vars '()
    :key :infile)
   (clingon:make-option
    :string
    :description "the output folder to write the results to"
    :short-name #\o
    :long-name "outfolder"
    :required t
    :env-vars '()
    :key :outfile)
   (clingon:make-option
    :integer
    :description "the amount of threads to use for the validation"
    :short-name #\t
    :long-name "threads"
    :required t
    :env-vars '()
    :key :threads)))


(defun validate/handler (cmd)
  "Handler for the `validate' command"
  (let ((in (clingon:getopt cmd :infile))
	(out (clingon:getopt cmd :outfile))
	(threads (clingon:getopt cmd :threads)))
    (time (main-val in out threads))))

(defun validate/command ()
  "A command to validate someone"
  (clingon:make-command
   :name "validate"
   :description "validates data"
   :version "0.1.0"
   :authors '("Koen van den Berg <k.vandenberg@insertdata.nl>")
   :license "MIT"
   :options (validate/options)
   :handler #'validate/handler))

(defun main ()
  "The main entrypoint of our CLI program"
  (let ((app (validate/command)))
    (clingon:run app)))


