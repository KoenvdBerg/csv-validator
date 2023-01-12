;; This file contains the functions that enable the stream_validator
;; to run in parallel. 
(in-package :stream_validator)

;; Functions that split file and/or add index to file
(defun move-working-dir (target-dir)
  "Move the workind directory to target-dir"
  (sb-posix:chdir target-dir))

(defun split-file-in-n (nr-files infile outdir)
  "Splits the tabular infile in N=nr-files splits that can be processed individually"
  (move-working-dir outdir)
  (let ((command (format nil "split --number=l/~a ~a" nr-files infile)))
    (uiop:run-program command)))

(defun add-index-to-file (infile outdir)
  "Adds an index column to the infile at column position 0 (first column)"
  (let* ((outfile (format nil "~a~a" outdir "with_index.csv"))
	 (command (format nil "nl -s ',' -w 1 -v 0 ~a" infile)))
    (uiop:run-program command :output (pathname outfile))
    outfile))

;; initialise the kernel and parallel functions
(defun init (threads)
  "Initializes the lparallel kernel"
  (setf lparallel:*kernel* (lparallel:make-kernel threads :name "channel-queue-kernel")))

(defun shutdown ()
  "Shuts down the kernel"
  (lparallel:end-kernel :wait t))

