;; Este modulo define como correr este programma con mas subprocesos
(in-package :stream_validator)

;; Helper functions:
(defun count-lines (file &optional (buffer-size 32768))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum buffer-size))
  (let ((buffer
         (make-array buffer-size
                     :element-type #1='(unsigned-byte 8)))
        (sum 0)
        (end 0))
    (declare (type fixnum sum end))
    (with-open-file (in file :element-type #1#)
      (loop
         (setf end (read-sequence buffer in))
         (when (= end 0)
           (return sum))
         (dotimes (i end)
           (declare (type fixnum i)
                    (dynamic-extent i))
           (when (= 10
                    (aref buffer i))
             (incf sum)))))))

(defun move-working-dir (target-dir)
  (sb-posix:chdir target-dir))

(defun list-dir (outdir &optional (prefix "x"))
  (directory (format nil "~a~a*" outdir prefix)))

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


;; initialise the kernel and parallel functions
(defun init (threads)
  (setf lparallel:*kernel* (lparallel:make-kernel threads :name "channel-queue-kernel")))

(defun shutdown ()
  (lparallel:end-kernel :wait t))

