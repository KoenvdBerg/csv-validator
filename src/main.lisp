;; The entry point for the csv-validator

(in-package :csv-validator)

(defun validate-csv (in validation-suite &key mode)
  (cond
    ((equal mode 'header-only) (validate-csv-header-only in validation-suite))
    ((equal mode 'summary) (validate-csv-with-mode in validation-suite 'summary))
    ((equal mode 'complete) (validate-csv-with-mode in validation-suite 'complete))
    (t (error (format nil "this mode has not been implemented: ~a" mode)))))

