;; This file contains functions to validate headers and records

(in-package :csv-validator)



;;------------------ VALIDATION SUITE STRUCT --------------------

(defstruct validation-suite "defines the contents of a validation suite"
	   (name "")
	   (expected-n-columns 10)
	   (csv-config (csvconfig))
	   (validation-rules nil))

(defstruct rule "define a validation rule"
	   (name "")
	   (column "")
	   (depends nil)
	   (label "explanation upon error")
	   (logic (lambda () t)))


;;------------------ HEADER VALIDATION --------------------

(defun validate-csv-header-only (stream suite)
  (let* ((metrics (init-metrics suite))
	 (header (get-header-row stream suite)) ;; reads 1st line (=header)
    	 (results (validate-header header suite)))
    (setf (metrics-found-headers metrics) (car results))
    (setf (metrics-missing-headers metrics) (second results))
    metrics))

(defun get-header-row (stream suite)
  (csvline->vector (read-line stream nil :eof)
		   (validation-suite-expected-n-columns suite)
		   (validation-suite-csv-config suite)))


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
     :csv-config (validation-suite-csv-config suite)
     :validation-rules viable-rules)))

;;------------------ RECORD VALIDATION --------------------
	
(defun validate-csv-with-mode (stream suite mode)
  (let* ((metrics (init-metrics suite))
	 (header (get-header-row stream suite)) ;; reads 1st line of infile
    	 (results (validate-header header suite))
	 (viable-suite (get-viable-suite header suite)))
    (setf (metrics-found-headers metrics) (car results))
    (setf (metrics-missing-headers metrics) (second results))
    (setf (metrics-results metrics) (init-record-hash-table viable-suite))
    ;; record validation
    (loop for
	  line = (read-line stream nil :eof)
	  for idx from 0
	  until (eq line :eof) do
	    (let ((validated-record (validate-record line viable-suite header idx)))
	      (update-metrics metrics validated-record)
	      (update-metrics-table (metrics-results metrics) validated-record mode))
	  finally (setf (metrics-nlines metrics) idx))
    metrics))

(defun validate-record (line suite header idx)
  (let ((record (csvline->vector line
				 (validation-suite-expected-n-columns suite)
				 (validation-suite-csv-config suite))))
    (mapcar #'(lambda (rule) (apply-validation-rule rule record header idx))
	    (validation-suite-validation-rules suite))))

(defun apply-validation-rule (rule record header idx)
  (let ((logic (rule-logic rule))
	(name (rule-name rule))
	(vals (get-values header rule record)))
    (if (apply logic vals)
	`(:name ,name :pass? t :values ,vals :index ,idx)
	`(:name ,name :pass? nil :values ,vals :index ,idx))))

(defun get-values (header rule record)
  (let ((depends (rule-depends rule)))
    (loop for x in depends
	  collect (aref record (get-col-position x header)))))

(defun get-col-position (col-name cols)
  (declare (string col-name))
  (position col-name cols :test #'string=))
	      
  
