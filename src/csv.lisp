;; Contains functions that parse in csv files
;;
;; Code inspired from: https://github.com/ebobby/cl-simple-table

(in-package :csv-validator)

(defun split-string (separator str)
  "Splits a string using the given separator, returns a list with the substrings.

  args
  ----
  separator: character, char that is used to split the input csv with
  str: string, the input string that will be split

  returns
  ----
  list with elements between delimiter

  example
  ----
  name;age;gender --> (\"name\" \"age\" \"gender\")
  "
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
