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
    :enum
    :description "the validation suite to be validated with"
    :short-name #\s
    :long-name "validation-suite"
    :required t
    :env-vars '()
    :key :selected-suite
    :items   `(("medical" . ,*medical_suite*)
	       ("taxi" . ,*taxidata*)))
   (clingon:make-option
    :integer
    :description "the amount of threads to use for the validation"
    :short-name #\t
    :long-name "threads"
    :required nil
    :initial-value 1
    :env-vars '()
    :key :threads)
   (clingon:make-option
    :string
    :description "the csv delimiter (single character). Examples: [; , | + : #]"
    :short-name #\d
    :long-name "csv-delimiter"
    :required nil
    :initial-value ","
    :env-vars '()
    :key :csv-delim)))

(defun validate/handler (cmd)
  "Handler for the `validate' command"
  (let ((in (clingon:getopt cmd :infile))
	(out (clingon:getopt cmd :outfile))
	(suite (clingon:getopt cmd :selected-suite))
	(threads (clingon:getopt cmd :threads))
	(csv-delim (char (clingon:getopt cmd :csv-delim) 0)))
    (validate-main in out suite :threads threads :delim csv-delim)))

(defun validate/command ()
  "A command to validate someone"
  (clingon:make-command
   :name "validate"
   :description "validates data"
   :version "0.1.0"
   :authors '("Koen van den Berg <k.vandenberg@insertdata.nl>")
   :license "BSD"
   :options (validate/options)
   :handler #'validate/handler))

(defun main ()
  "The main entrypoint of our CLI program"
  (let ((app (validate/command)))
    (clingon:run app)))

(defun validate-main (in outdir validation-suite &key (threads 1) (delim #\,))
  "Main function that is used to apply the stream_validator to a dataset

  args
  ----
  in: filepath to the input csv file
  outdir: filepath to the output directory where the results will be stored
  validation-suite: the suite to use to validate the input csv to
  threads: integer that indicates the amount of threads to use. Best is between 1-4
  delim: the csv delimiter

  returns
  ----
  nil  
  "
  (let* ((in (add-index-to-file in delim outdir))
	 (header (get-header-row in delim))
	 (suite (validate-header header validation-suite))
	 (missing-header (set-difference validation-suite suite)))
    (cond ((<= threads 1) (run-record-validation in outdir header suite delim))
	  ((> threads 1)
	   ;; If threads is greater than 1, the infile is split into divisions,
	   ;; 1 division for each thread. The thread can then run autonomously.
	   (split-file-in-n threads in outdir)
	   (init threads)
	   (let ((channel (lparallel:make-channel)))
	     (loop for f in (list-dir outdir)
		   do (lparallel:submit-task channel #'run-record-validation
					     f outdir header suite delim))
	     (lparallel:receive-result channel))
	   (shutdown)))
    (write-result-file missing-header outdir)
    (cleanup-outdir outdir)))
