;; The entry point for the csv-validator

(in-package :csv-validator)

(defun validate-csv (in outdir validation-suite &key (threads 1) (delim #\,))
  "Main function that is used to apply the csv-validator to a dataset

  args
  ----
  in: string, filepath to the input csv file
  outdir: string, filepath to the output directory where the results will be stored
  validation-suite: the suite to use to validate the input csv to
  threads: integer, indicates the amount of threads to use. Best is between 1-4
  delim: character, the csv delimiter

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
