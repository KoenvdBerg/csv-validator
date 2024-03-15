;; Contains functions that parse in csv files

(in-package :csv-validator)

;; todo: move this to another spot later
(defun getlines (infile)
  (with-open-file (stream infile)
    (loop for
	  line = (read-line stream nil :eof)
	  until (eq line :eof)
	  do (csvline->vector line 20))))


;;------------------------- PUBLIC  -------------------------
(defun csvline->vector (line ncolumns &optional (configuration (csvconfig)))
  "parses a line of csv to a vector given the csv-configuration"
  (declare (string line))
  (declare (fixnum ncolumns))
  (let ((cur (cursor ncolumns line)))
    (cursor-ret (parse-segments cur configuration))))

(defun csvconfig (&optional (delim-char #\;) (quote-char #\") (escape-char #\\))
  "create a csv-configuration struct"
  (make-config :delim delim-char :quote-char quote-char :escape-char escape-char))

;;------------------------- STRUCTS -------------------------
(defstruct cursor "the cursor for parsing a csv line"
	   (idx 0) (line "") (ret (make-array 0)))

(defun cursor (ncolumns csv-line)
  (make-cursor :idx 0 :line csv-line :ret (make-array ncolumns :fill-pointer 0)))


(defstruct config "the configuration for the csv-parser"
	   delim quote-char escape-char)

;;------------------------- PARSER -------------------------
(defun parse-segments (cur configuration)
  (let* ((upper (next-delim configuration
			   (cursor-line cur)
			   (cursor-idx cur)))
	 (slice (sliceseg cur (second upper)))
	 (found (strip-quote-chars slice (config-quote-char configuration))))
    (vector-push found (cursor-ret cur))
    (if (equal (car upper) 'continue) ;; not yet at end of line, continue
	(progn
	  (setf (cursor-idx cur) (+ (second upper) 1))
	  (parse-segments cur configuration))
	cur)))

(defun is-escaped-delim (line idx configuration)
  (if (<= idx 0)
      nil
      (let ((prev-char (aref line (- idx 1))))
	(equal (config-escape-char configuration) prev-char))))

(defun next-delim (configuration line from)
  (let ((hit (position (config-delim configuration) line :test #'equal :start from)))
    (if hit
	(if (is-escaped-delim line hit configuration)
	    ;; try again from next startpoint
	    (next-delim configuration line (+ hit 1))
	    `(continue ,hit))
	;; the case when no hit is found return last line idx
	`(eol ,(length line)))))

(defun sliceseg (cur end)
  (subseq (cursor-line cur) (cursor-idx cur) end))

(defun strip-quote-chars (segment quote-char)
  (if (<= (length segment) 0)
      ""
      (let ((fst (aref segment 0))
	    (lst (aref segment (- (length segment) 1))))
	(if (and (equal fst quote-char) (equal lst quote-char))
	    (subseq segment 1 (- (length segment) 1))
	    segment))))

