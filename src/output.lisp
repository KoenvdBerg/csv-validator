;; This file contains the functions that manipulate the output folder
;; and create the final validation result file. The name for this file is:
;;
;; errors.csv
;;
;; By: Koen van den Berg
(in-package :stream_validator)

(defun list-dir (outdir &optional (prefix "x*"))
  "List files in specified outdir that have wildcard prefix"
  (directory (format nil "~a~a" outdir prefix)))

(defun join-files-together (filepaths outdir)
  "Appends multiple csv files together vertically to new file called errors.csv"
  (let* ((outfile (format nil "~astream_validator_validations.csv" outdir))
	 (command (format nil "cat ~{~a ~}" filepaths)))
    (uiop:run-program command :output (pathname outfile))
    (pathname outfile)))

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

(defun write-result-header-file (outdir)
  "Writes the header row of the validation result file"
  (let ((header-file (format nil "~atmp_headerrow.csv" outdir)))
    (with-open-file (outstr header-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format outstr "index;column;erronuous_value;message~%"))
    (pathname header-file)))

(defun write-header-validation-file (missing-header outdir)
  (let ((header-validation-file (format nil "~atmp_header_validation.csv" outdir)))
    (with-open-file (outstr header-validation-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (loop for spec in missing-header
	    for column = (getf spec :column)
	    for message = (getf spec :message)
	    do (format outstr "~a;~a;~a;~a~%" 0 column column message)))
    header-validation-file))

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
