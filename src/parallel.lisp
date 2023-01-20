;; This file contains the functions that enable the csv-validator
;; to run in parallel. 
(in-package :csv-validator)


;; initialise the kernel and parallel functions
(defun init (threads)
  "Initializes the lparallel kernel"
  (setf lparallel:*kernel* (lparallel:make-kernel threads :name "channel-queue-kernel")))

(defun shutdown ()
  "Shuts down the kernel"
  (lparallel:end-kernel :wait t))

