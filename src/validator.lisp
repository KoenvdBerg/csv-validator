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

;; todo: move this elsewhere
(defun get-header-row (stream suite)
  (let ((header-row (csvline->vector (read-line stream nil :eof)
		   (validation-suite-expected-n-columns suite)
		   (validation-suite-csv-config suite))))
    `(,header-row ,stream)))

(defun rule-can-be-appliedp (rule header)
  (let* ((depends (rule-depends rule))
	 (column (rule-column rule))
	 (to-check (remove-duplicates (concatenate 'list (list column) depends)))
	 (found-columns (mapcar
			 #'(lambda (col) (get-col-position col header))
			 to-check)))
    (notany #'null found-columns)))
	
(defun filter-suite-present-cols (header suite)
  (remove-if-not #'(lambda (rule) (rule-can-be-appliedp rule header))
		 (validation-suite-validation-rules suite)))


;;------------------ RECORD VALIDATION --------------------

(defun validate-with-metrics (in suite)
  (let ((metrics nil))
    (with-open-file (stream in)
      (let ((header (get-header-row stream suite)))
	(setf stream (second header))  ;; get-header-row reads 1st line
	(loop for line = (read-line stream nil :eof) until (eq line :eof) do
	  ;; todo: update metrics and finally return
	  (validate-record line suite (car header)))))))


(defun validate-record (line suite header)
  (let ((record (csvline->vector line
				 (validation-suite-expected-n-columns suite)
				 (validation-suite-csv-config suite))))
    (when (not (equalp header record))
      (mapcar #'(lambda (rule) (apply-validation-rule rule record header))
	      (validation-suite-validation-rules suite)))))

(defun apply-validation-rule (rule record header)
  (let ((logic (rule-logic rule))
	(vals (get-values header rule record)))
    (if (apply logic vals)
	t
	nil)))

(defun get-values (header rule record)
  (let ((depends (rule-depends rule)))
    (loop for x in depends
	  collect (aref record (get-col-position x header)))))

(defun get-col-position (col-name cols)
  (declare (string col-name))
  (position col-name cols :test #'string=))
	      
  
