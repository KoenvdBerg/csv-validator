;; The entry point for the csv-validator

(in-package :csv-validator)

(defun validate-csv-file (infile validation-suite &key mode)
  (with-open-file (stream infile)
    (validate-csv stream validation-suite mode)))

(defun validate-csv-string (csv-string validation-suite &key mode)
  (with-input-from-string (stream csv-string)
    (validate-csv stream validation-suite mode)))

(defun validate-csv (stream validation-suite mode)
  (cond
    ((string= mode 'header-only) (validate-csv-header-only stream validation-suite))
    ((string= mode 'summary) (validate-csv-with-mode stream validation-suite 'summary))
    ((string= mode 'complete) (validate-csv-with-mode stream validation-suite 'complete))
    (t (error (format nil "this mode has not been implemented: ~a" mode)))))
