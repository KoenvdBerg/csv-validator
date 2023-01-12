;; Este modulo define como correr este programma con mas subprocesos
(in-package :stream_validator)

;; Helper functions:
(defun move-working-dir (target-dir)
  (sb-posix:chdir target-dir))

(defun list-dir (outdir &optional (prefix "x*"))
  (directory (format nil "~a~a" outdir prefix)))

(defun split-file-in-n (nr-files infile outdir)
  "divide el archivo out en differentes divisiones basado en el parameter nr-files"
  (move-working-dir outdir)
  (let ((command (format nil "split --number=l/~a ~a" nr-files infile)))
    (uiop:run-program command)))

(defun add-index-to-file (infile outdir)
  (let* ((outfile (format nil "~a~a" outdir "with_index.csv"))
	 (command (format nil "nl -s ',' -w 1 -v 0 ~a" infile)))
    (uiop:run-program command :output (pathname outfile))
    outfile))

(defun cleanup-outdir (outdir)
  ;; cleanup the splits (if present)
  (let ((to-cleanup (list-dir outdir "x*")))
    (loop for f in to-cleanup do
      (delete-file f)))
  ;; cleanup the worker results (if present)
  (let ((to-cleanup (list-dir outdir "result_*.csv")))
    (loop for f in to-cleanup do
      (delete-file f)))
  (delete-file (car (list-dir outdir "header.csv"))))

(defun write-header-file (outdir)
  (let ((header-file (format nil "~aheader.csv" outdir)))
    (with-open-file (outstr header-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format outstr "index;column;erronuous_value;message~%"))
    (pathname header-file)))

(defun join-files-together (filepaths outdir)
  (let* ((outfile (format nil "~aerrors.csv" outdir))
	 (command (format nil "cat ~{~a ~}" filepaths)))
    (uiop:run-program command :output (pathname outfile))
    (pathname outfile)))

(defun write-result-file (outdir)
  (let* ((header-file (write-header-file outdir))
	 (result-files (list-dir outdir "result_*.csv"))
	 (to-join (append (list header-file) result-files)))
    (join-files-together to-join outdir)))

;; initialise the kernel and parallel functions
(defun init (threads)
  (setf lparallel:*kernel* (lparallel:make-kernel threads :name "channel-queue-kernel")))

(defun shutdown ()
  (lparallel:end-kernel :wait t))

