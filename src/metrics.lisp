;; this file contains the definitions to compute validation metrics


(in-package :csv-validator)


;;------------------ METRICS STRUCT --------------------

(defstruct metrics "holds data for final validation metrics to report as result"
	   (suite-name "")
	   (nlines 0)
	   (npass 0)
	   (nfail 0)
	   (found-headers nil)
	   (missing-headers nil)
	   (results (make-hash-table))) ;; todo make sure to size the hash table to number of rules in validation suite

(defstruct rule-metrics "holds metrics for one validation rule"
	   (rule-name "")
	   (rule-label "")
	   (npass 0)
	   (nfail 0)
	   (indices #())
	   (values #()))
	   
(defun init-metrics (suite)
  (make-metrics
   :suite-name (validation-suite-name suite)
   :nlines 0
   :npass 0
   :nfail 0
   :found-headers nil
   :missing-headers nil
   :results nil))

(defun init-rule-metrics (rule)
  (make-rule-metrics
   :rule-name (rule-name rule)
   :rule-label (rule-label rule)
   :npass 0
   :nfail 0
   :indices (make-array 100 :fill-pointer 0 :adjustable t)
   :values (make-array 100 :fill-pointer 0 :adjustable t)))

(defun init-record-hash-table (suite)
  (let* ((rules (validation-suite-validation-rules suite))
	 (n (length rules))
	 (table (make-hash-table :size n :test 'equal)))
    (mapc
     #'(lambda (rule) (setf (gethash (rule-name rule) table) (init-rule-metrics rule)))
     rules)
    table))
    
   

(defun combine (m1 m2)
  nil)
