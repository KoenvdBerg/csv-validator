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
    (time (validate-main in out threads))))

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

(defun validate-main (in outdir validation-suite &key (threads 1))
  ;; saca la fila header y suite que funciona
  (let* ;; ((in (add-index-to-file in outdir))			 
	 ((header (get-header-row in))
	 (suite (header-suite-works header validation-suite)))
    ;; escribir el archivo out y apremiar el header del csv en out
    ;; (format str "index;error_value;error_message;column~%")
    (cond ((<= threads 1) (run-validation in outdir header suite))
	  ((> threads 1)
	   ;; divide el archivo in en n particiones:
	   (split-file-in-n threads in outdir)
	   ;; init workers que procesan cada archivo
	   (init threads)
	    (let ((channel (lparallel:make-channel)))
	      (loop for f in (list-dir outdir)
		    ;; for idx from 0
		    do (lparallel:submit-task channel #'run-validation
					      f outdir header suite))
	      (lparallel:receive-result channel))
	   (shutdown)
	   (cleanup-splits outdir)))))
