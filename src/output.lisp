;; This file contains the functions that manipulate the output folder
;; and create the final validation result file. The name for this file is:
;;
;; errors.csv
;;
;; By: Koen van den Berg
(in-package :stream_validator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEADER VALIDATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-header-validation-file (missing-header outdir)
  (let ((header-validation-file (format nil "~atmp_header_validation.csv" outdir))
	(missing-cols (remove-duplicates (loop for spec in missing-header
			    collect (getf spec :column)))))
    (with-open-file (outstr header-validation-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (loop for col in missing-cols
	    for message = (format nil "The column ~a is missing from the input data" col)
	    do (format outstr "~a;~a;~a;~a~%" 0 col col message)))
    header-validation-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECORD VALIDATION 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-result-header-file (outdir)
  "Writes the header row of the validation result file"
  (let ((header-file (format nil "~atmp_headerrow.csv" outdir)))
    (with-open-file (outstr header-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format outstr "index;column;erronuous_value;message~%"))
    (pathname header-file)))

(defun write-result-file (missing-header outdir)
  "Writes individual validation result files and combines them together"
  (let* ((result-header-file (write-result-header-file outdir))
	 (header-validation-file
	   (write-header-validation-file missing-header outdir))
	 (result-files (list-dir outdir "*validation_*.csv"))
	 (to-join (append (list result-header-file) ;header row 1st 
			  (list header-validation-file)
			  result-files))) 
    (join-files-together to-join outdir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-working-dir (target-dir)
  "Move the workind directory to target-dir"
  (sb-posix:chdir target-dir))

(defun split-file-in-n (nr-files infile outdir)
  "Splits the tabular infile in N=nr-files splits that can be processed individually"
  (move-working-dir outdir)
  (let ((command (format nil "split --number=l/~a ~a" nr-files infile)))
    (uiop:run-program command)))

(defun add-index-to-file (infile delim outdir)
  "Adds an index column to the infile at column position 0 with correct delimiter (first column)"
  (let* ((outfile (format nil "~a~a" outdir "tmp_infile_with_index.csv"))
	 (command (format nil "nl -s '~a' -w 1 -v 0 ~a" delim infile)))
    (uiop:run-program command :output (pathname outfile))
    outfile))

(defun list-dir (outdir &optional (prefix "x*"))
  "List files in specified outdir that have wildcard prefix"
  (directory (format nil "~a~a" outdir prefix)))

(defun join-files-together (filepaths outdir)
  "Appends multiple csv files together vertically to new file called errors.csv"
  (let* ((outfile (format nil "~astream_validator_validations.csv" outdir))
	 (command (format nil "cat ~{~a ~}" filepaths)))
    (uiop:run-program command :output (pathname outfile))
    (pathname outfile)))

(defun cleanup-outdir (outdir)
  "Cleans up the outdir"
  ;; cleanup the splits (if present)
  (let ((to-cleanup (list-dir outdir "x*")))
    (loop for f in to-cleanup do
      (delete-file f)))
  ;; cleanup the worker results (if present)
  (let ((to-cleanup (list-dir outdir "tmp_*.csv")))
    (loop for f in to-cleanup do
      (delete-file f))))
       
