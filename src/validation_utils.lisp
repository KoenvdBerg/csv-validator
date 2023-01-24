;; This file contains the utilities that are used to validate data.

(in-package :csv-validator)
  
;; data type validations
(defun check-integer-string (x)
  "Checks if the incoming string is an integer

  returns
  ----
  t if integer, else nil
  "
  (declare (type string x))
  (when (check-not-null x)
    (let* ((is-negative (string= (subseq x 0 1) "-")))
      (if is-negative
	  (every #'digit-char-p (subseq x 1))
	  (every #'digit-char-p x)))))


(defun check-float-string (x)
  "Checks if the incoming string is a float

  returns
  ----
  t if float, else nil
  "
  (declare (type string x))
  (let ((has-dot (search "." x)))
    (if has-dot
	(and (check-integer-string (subseq x 0 has-dot))
	     (check-integer-string (subseq x (1+ has-dot) (length x))))
	nil)))


(defun check-scientific-number-string (x)
  "Checks if the incoming string is a scientific number

  returns
  ----
  t if float, else nil
  "
  (declare (type string x))
  (let ((has-e (search "e" (string-downcase x))))
    (if has-e
	(and (or (check-integer-string (subseq x 0 has-e))
		 (check-float-string (subseq x 0 has-e)))
	     (check-integer-string (subseq x (1+ has-e) (length x))))
	nil)))


(defun check-number-string (x)
  "Checks in the incoming string is either an integer or a float

  returns
  ----
  t if number, else nil
  "
  (or (check-float-string x)
      (check-integer-string x)
      (check-scientific-number-string x)))


(defun check-date-parsable (x)
  "Checks if the incoming string is parsable as a date.

  returns
  ----
  date-object if parsable, else nil
  "
  (declare (type string x))
  (if (< (length x) 10)
      nil
      (local-time:parse-timestring x :start 0 :end 10 :fail-on-error nil)))


(defun check-tz-parsable (x)
  "Checks if the incoming string is parsable as a timezone date.

  returns
  ----
  date-object if parsable, else nil
  "
  (declare (type string x))
  (if (not (= (length x) 20))
      nil
      (local-time:parse-timestring x :start 0 :end 20 :fail-on-error nil)))


(defun check-null (x)
  "Checks if the incoming string can be considered null or null-like

  returns
  ----
  t if null, else nil
  "
  (declare (type string x))
  (let ((xl (string-downcase x)))
    (or (string= xl "")
	(string= xl "na")
	(string= xl "nan")
	(string= xl "-")
	(string= xl "null"))))

(defun check-not-null (x)
  "Checks if the incoming string is not null or null-like"
  (not (check-null x)))


;; date checks
(defun check-date-before-today (x)
  "Checks if the incoming string is a date that is before today

  returns
  ----
  nil if x is parsable and not before today
  t if x ain't parsable or before today
  "
  (declare (type string x))
  (or (not (check-date-parsable x))
      (local-time:timestamp<=
       (check-date-parsable x)
       (local-time:now))))

(defun check-compare-two-dates (x y)
  "Checks if the incoming date string x is before y

  returns
  ----
  nil if x > y
  t if x < y or either x or y not date parsable
  "
  (declare (type string x))
  (or (not (check-date-parsable x))
      (not (check-date-parsable y))
      (local-time:timestamp>=
       (check-date-parsable x)
       (check-date-parsable y))))

;; number checks
(defun check-integer-in-range (x range-start range-end)
  " Checks if the incoming string is an integer between range-start and
  range-end. If no range-start, checks if x < range-end. If no
  range-end, checks x > range-start.

  returns
  ----
  nil if x outside of range
  t if x isn't integer or within range
"
  (if (check-integer-string x)
      (let ((pint (parse-integer x)))
	(cond ((null range-start) (< pint range-end))
	      ((null range-end) (> pint range-start))
	      (t (and (< pint range-end) (> pint range-start)))))
      nil))

(defun check-number-in-range (x range-start range-end)
  " Checks if the incoming string is a number between range-start and
  range-end. If no range-start, checks if x < range-end. If no
  range-end, checks x > range-start.

  returns
  ----
  nil if x outside of range
  t if x isn't integer or within range
"
  (if (check-number-string x)
      (let ((pint (parse-float:parse-float x)))
	(cond ((null range-start) (< pint range-end))
	      ((null range-end) (> pint range-start))
	      (t (and (< pint range-end) (> pint range-start)))))
      nil))
