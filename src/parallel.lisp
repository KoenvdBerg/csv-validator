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
    (trivial-shell:shell-command command)))

(defun add-index-to-file (infile outdir)
  (let* ((outfile (format nil "~a~a" outdir "with_index.csv"))
	 (command (format nil "nl -s ',' -w 1 -v 0 ~a > ~a" infile outfile)))
    (trivial-shell:shell-command command)
    outfile))

(defun cleanup-splits (outdir)
  (let ((to-cleanup (list-dir outdir)))
    (loop for f in to-cleanup do
      (delete-file f))))

(defun join-splits-together (outdir)
  ;; write the header to the final result
  (let ((header-file (format nil "~aheader.csv" outdir)))
    (with-open-file (outstr header-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format outstr "index;column;erronuous_value;message~%"))
    ;; join all the files together
    (let* ((to-join (append (list header-file)
			    (list-dir outdir "result_*.csv")))
	   (outfile (format nil "~aerrors.csv" outdir))
	   (command (format nil "cat ~{~a ~} > ~a" to-join outfile)))
      (print command)
      (trivial-shell:shell-command command)
      (loop for f in to-join do
	(delete-file f))
      "errors.csv")))


;; initialise the kernel and parallel functions
(defun init (threads)
  (setf lparallel:*kernel* (lparallel:make-kernel threads :name "channel-queue-kernel")))

(defun shutdown ()
  (lparallel:end-kernel :wait t))

