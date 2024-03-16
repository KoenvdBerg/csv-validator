;; This file contains functions to validate headers and records

(in-package :csv-validator)



;;------------------ VALIDATION SUITE STRUCT --------------------

(defstruct validation-suite "defines the contents of a validation suite"
	   (name "")
	   (expected-n-columns 10)
	   (n-columns-wiggle 5)
	   (csv-config (csvconfig))
	   (validation-rules nil))

(defstruct rule "define a validation rule"
	   (name "")
	   (column "")
	   (depends nil)
	   (label "explanation upon error")
	   (logic (lambda () t)))


;;------------------ HEADER VALIDATION --------------------

(defun validate-csv-header-only (in suite)
  (let ((metrics (init-metrics suite)))
    (with-open-file (stream in)
      ;; header validation
      (let* ((header (car (get-header-row stream suite)))
    	     (results (validate-header header suite)))
	(setf (metrics-found-headers metrics) (car results))
	(setf (metrics-missing-headers metrics) (second results))))
    metrics))

(defun get-header-row (stream suite)
  (let ((header-row (csvline->vector (read-line stream nil :eof)
		   (validation-suite-expected-n-columns suite)
		   (validation-suite-csv-config suite))))
    `(,header-row ,stream)))

(defun validate-header (header suite)
  (let* ((found-headers (get-viable-headers header suite))
	 (expected-headers (mapcar
			    #'rule-column
			    (validation-suite-validation-rules suite)))
	 (missing (set-difference expected-headers found-headers :test #'equal)))
    `(,found-headers ,missing)))

(defun rule-can-be-appliedp (rule header)
  (let* ((depends (rule-depends rule))
	 (column (rule-column rule))
	 (to-check (remove-duplicates (concatenate 'list (list column) depends)))
	 (found-columns (mapcar
			 #'(lambda (col) (get-col-position col header))
			 to-check)))
    (notany #'null found-columns)))
	
(defun get-viable-rules (header suite)
  (remove-if-not #'(lambda (rule) (rule-can-be-appliedp rule header))
		 (validation-suite-validation-rules suite)))

(defun get-viable-headers (header suite)
  (mapcar #'rule-column (get-viable-rules header suite)))

(defun get-viable-suite (header suite)
  (let ((viable-rules (get-viable-rules header suite)))
    (make-validation-suite
     :name (validation-suite-name suite)
     :expected-n-columns (validation-suite-expected-n-columns suite)
     :n-columns-wiggle (validation-suite-n-columns-wiggle suite)
     :csv-config (validation-suite-csv-config suite)
     :validation-rules viable-rules)))

;;------------------ RECORD VALIDATION --------------------
	
(defun validate-csv-with-mode (in suite mode)
  (let ((metrics (init-metrics suite)))
    (with-open-file (stream in)
      ;; header validation (reads 1st line of infile)
      (let* ((header (get-header-row stream suite))
    	     (results (validate-header (car header) suite))
	     (viable-suite (get-viable-suite (car header) suite)))
	(setf (metrics-found-headers metrics) (car results))
	(setf (metrics-missing-headers metrics) (second results))
	(setf (metrics-results metrics) (init-record-hash-table viable-suite))
	(setf stream (second header))  ;; get-header-row reads 1st line
	;; record validation
	(loop for line = (read-line stream nil :eof) for idx from 0 until (eq line :eof) do
	  ;; todo: update metrics and finally return
	  ;; todo: make validate-record return rule-name
	  ;; tood: add cond mode for what to add to the metrics
	  (print (validate-record line viable-suite (car header)))
	  finally (setf (metrics-nlines metrics) idx))))
    metrics))

(defun validate-record (line suite header)
  (let ((record (csvline->vector line
				 (validation-suite-expected-n-columns suite)
				 (validation-suite-csv-config suite))))
    (mapcar #'(lambda (rule) (apply-validation-rule rule record header))
	    (validation-suite-validation-rules suite))))

(defun apply-validation-rule (rule record header)
  (let ((logic (rule-logic rule))
	(name (rule-name rule))
	(vals (get-values header rule record)))
    (if (apply logic vals)
	`(,name pass)
	`(,name fail))))

(defun get-values (header rule record)
  (let ((depends (rule-depends rule)))
    (loop for x in depends
	  collect (aref record (get-col-position x header)))))

(defun get-col-position (col-name cols)
  (declare (string col-name))
  (position col-name cols :test #'string=))
	      
  
