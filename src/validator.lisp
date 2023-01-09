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

(defun get-col-position (col-name cols)
  "Saca la posicion (en numero) de una columna por su nombre"
  (position col-name cols :test #'string=))

(defun get-values (header spec record)
  "saca los valores de un record basado en el spec del suite"
  (let ((depends (getf spec :depends)))
    (loop for x in depends
	  collect (nth (get-col-position x header) record))))

(defun validate (vals spec index)
  "validaion de los datos pasa aqui"
  (let ((message (getf spec :message))
	(column (getf spec :column))
	(logic (getf spec :logic))
	(error-val (car vals)))
    (when (not (apply logic vals))
	(format t "~a;~a;~a;~a~%" index error-val message column))))

(defun get-header-row (infile)
  "saca la fila header del csv infile"
  (with-open-file (stream infile)
    (car (cl-csv:read-csv (read-line stream nil :eof)))))

(defun header-suite-works (header suite)
  "aproba si la fila header este representada dentro del suite. El valor
de retorno es una lista con los spec que funcionan"
  (loop for spec in suite
	for depends = (getf spec :depends)
	for column = (getf spec :column)
	for to-check = (remove-duplicates
			(concatenate 'list (list column) depends))
	for col-positions = (loop for c in to-check
				  collect (get-col-position c header) into res
				  finally (return res)) 
	collect (when (notany #'null col-positions)
		  spec)))

(defun run-validation (in outdir header suite)
  "funcion main"
  (with-open-file (stream in)
    (with-open-file (*standard-output* (format nil "~aresult_~a.csv" outdir (gensym))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      ;; un bucle sobre las filas del csv.
      (loop for line = (read-line stream nil :eof)
	    until (eq line :eof) do
	      ;; hace que la fila header no esta processada
	      (let* ((record (car (cl-csv:read-csv line)))
		     (idx (car record)))
		(cond ((not (equal record header))
		       ;; por cada spec in suite, haz la validacion
		       (loop for spec in suite do
			 (let ((vals (get-values header spec record)))
			   (validate vals spec idx))))))))))
