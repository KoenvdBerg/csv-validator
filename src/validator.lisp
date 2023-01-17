;; This package validates data
;;
;; Author: Koen van den Berg
;;
;; ideas:
;; 
;; Es possible que hago una pieza de codigo que prueba los headers de un
;; CSV archivo. Si los headers no son exactamente lo mismo que lo que
;; esta definido en el base de datos de las validaciones, da un
;; error. Puedo usar el mismo codigo que usare para hacer validaciones
;; para los demas datos en las filas.
;;
;; Si una validacion da un error, este escrita a los resultados que
;; incluya las columnas: nombre_columna, valor_equivocado, index,
;; logica, mensaje_de_error. --> los valores para logica y mensaje de
;; error son definidos en el 'suite' de validacion.
(in-package :stream_validator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEADER VALIDATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-header-row (infile delim)
  "Obtains the header row from the infile

  args
  ----
  infile: filepath to the input csv file
  delim: the csv delimiter

  returns
  ----
  header row as a list. E.g. '(\"header1\" \"header2\")
  "
  (declare (type string infile)
	   (type character delim))
  (with-open-file (stream infile)
    (to-record (split-string delim (read-line stream nil :eof)))))

(defun validate-header (header suite)
  "Validates the header against the expectation suite. Filters the
  expectation suite so that only the specifications remain that are
  also present in the header.

  args
  ----
  header: list that contains the headers as strings
  suite: the suite to use to validate the input csv to

  returns
  ----
  suite but then filtered so that it contains only the specs that are
  also in header
  "
  (loop for spec in suite
	for depends = (getf spec :depends)
	for column = (getf spec :column)
	for to-check = (remove-duplicates
			(concatenate 'list (list column) depends))
	for col-positions = (loop for c in to-check
				  collect (get-col-position c header) into res
				  finally (return res))
	when (notany #'null col-positions)
	  collect spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECORD VALIDATION 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-col-position (col-name cols)
  "Obtains the position of a column name in a list of columns

  args
  ----
  col-name: string that describes the column name
  cols: list of column names to find the position in

  returns
  ----
  int column position
  "
  (declare (type string col-name))
  (position col-name cols :test #'string=))

(defun get-values (header spec record)
  "Obtains the values from a record based on the depends field in the
  validation suite

  args
  ----
  header: list that contains the headers as strings
  spec: a single specification of a validation suite as a list
  record: list that holds the values of that record. Every value is string.

  returns
  ----
  list of values
  "
  (let ((depends (getf spec :depends)))
    (loop for x in depends
	  collect (aref record (get-col-position x header)))))

(defun validate-record (vals spec)
  "Validates record-values based on the logic defined in a single
  specification of a validation suite

  args
  ----
  vals: list of values. Every value is string
  spec: a single specification of a validation suite as a list

  returns
  ----
  t if logic holds, else nil
  "
  (let ((logic (getf spec :logic)))
    (if (apply logic vals)
	t
	nil)))
	
(defun run-record-validation (in outdir header suite delim)
  "Main function for validating a single csv infile in 1 thread

  args
  ----
  in: filepath to the input csv file
  outdir: filepath to the output directory where the results will be stored
  suite: the suite to use to validate the input csv to
  delim: the csv delimiter

  returns
  ----
  nil
  "
  ;; first open infile and create outfile with unique filename
  (with-open-file (stream in)
    (with-open-file (outstream
		     (format nil "~atmp_record_validation_id=~a.csv"
			     outdir (gensym))
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
      ;; parse in each line to a record 
      (loop for line = (read-line stream nil :eof)
	    until (eq line :eof) do
	      ;; skip the header row
	      (let* ((record (to-record (split-string delim line)))
		     (idx (aref record 0)))
		(cond ((not (equalp record header))
		       ;; for every spec in the validation-suite,
		       ;; perform the validation. Write result if
		       ;; validation returned true.
		       (loop for spec in suite do
			 (let ((vals (get-values header spec record)))
			   (when (not (validate-record vals spec))
			     (write-record-to-stream outstream idx spec vals)))))))))))
