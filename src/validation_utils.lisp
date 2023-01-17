;; This file contains the utilities that are used to validate data.
;;
;; By: Koen van den Berg

(in-package :stream_validator)
  
;; data type validations
(defun check-integer-string (x)
  (let* ((is-negative (string= (subseq x 0 1) "-")))
    (if is-negative
	(every #'digit-char-p (subseq x 1))
	(every #'digit-char-p x))))

(defun check-date-parsable (x)
  "returns nil if not parsable, returns parsed date if parsable"
  (chronicity:parse x :endian-preference :little))

(defun check-null (x)
  (let ((xl (string-downcase x)))
    (or (string= xl "")
	(string= xl "na")
	(string= xl "nan")
	(string= xl "null"))))

(defun check-not-null (x)
  (not (check-null x)))


;; date checks
(defun check-date-before-today (x)
  (or (not (check-date-parsable x))
      (local-time:timestamp<=
       (check-date-parsable x)
       (check-date-parsable "today"))))

(defun check-compare-two-dates (x y)
  (or (not (check-date-parsable x))
      (not (check-date-parsable y))
      (local-time:timestamp>=
       (check-date-parsable x)
       (check-date-parsable y))))

;; integer checks
(defun check-integer-in-range (x range-start range-end)
  (if (check-integer-string x)
      (let ((pint (parse-integer x)))
	(cond ((null range-start) (< pint range-end))
	      ((null range-end) (> pint range-start))
	      (t (and (< pint range-end) (> pint range-start)))))
      nil))
