;; Contains functions that parse in csv files
;;
;; Shoutout to: https://github.com/ebobby/cl-simple-table

(in-package :stream_validator)

;; funciones que sirven para cargar datos del archivo csv
;; https://github.com/ebobby/cl-simple-table
(defun split-string (separator str)
  "Splits a string using the given separator, returns a list with the substrings."
  (declare (type character separator)
           (type string str))
  (loop
     with len = (length str)
     for fr = 0 then (1+ in)
     while (<= fr len)
     for in = (or (position separator str :test #'char= :start fr) len)
     collect (subseq str fr in)))

(defun to-record (elements)
  "Converts a sequence of elements into a record."
  (coerce elements 'record))

(deftype record ()
  "record type."
  `(vector t *))
