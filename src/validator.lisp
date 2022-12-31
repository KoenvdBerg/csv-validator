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

;; (defparameter *suitetest*
;;   (list
;;    (list
;;     :column "count"
;;     :type "integer"
;;     :depends (list "count")
;;     :message "El valor tiene que ser un numero"
;;     :logic (lambda (count) (not (ignore-errors (parse-integer count)))))
;;    (list
;;     :column "order"
;;     :type "string"
;;     :depends (list "order" "name")
;;     :message "La columna es vacio"
;;     :logic (lambda (x0 x1) (and (string= x0 "") (string= x1 "Chris"))))
;;    (list
;;     :column "city"
;;     :type "string"
;;     :depends (list "city")
;;     :message "el nombre no puede ser Manchester"
;;     :logic (lambda (x) (string= x "Manchester")))
;;    (list
;;     :column "date"
;;     :type "date"
;;     :depends (list "date")
;;     :message "La fecha no puede ser en el futuro"
;;     :logic (lambda (x) (local-time:timestamp>=
;; 			(local-time:parse-timestring x)
;; 			(local-time:now))))
;;    (list
;;     :column "date"
;;     :type "date"
;;     :depends (list "date")
;;     :message "La fecha no puede ser antes de 1990-01-01"
;;     :logic (lambda (x) (local-time:timestamp<=
;; 			(local-time:parse-timestring x)
;; 			(local-time:parse-timestring "1990-01-01"))))
;;    (list
;;     :column "order"
;;     :type "string"
;;     :depends (list "order" "name" "city")
;;     :message "La columna tiene que ser llena si has Chris y Manchester"
;;     :logic (lambda (x0 x1 x2) (and (string= x0 "")
;; 				   (and (string= x1 "Chris")
;; 					(string= x2 "Manchester")))))))


(defun get-col-position (col-name cols)
  "Saca la posicion (en numero) de una columna por su nombre"
  (position col-name cols :test #'string=))

(defun get-values (header spec record)
  "saca los valores de un record basado en el spec del suite"
  (let ((depends (getf spec :depends)))
    (loop for x in depends
	  collect (nth (get-col-position x header) record)
	    into record-values
	  finally (return record-values))))
				    
(defun validate (vals spec outstream index)
  "validaion de los datos pasa aqui"
  (let ((message (getf spec :message))
	(column (getf spec :column))
	(logic (getf spec :logic))
	(error-val (car vals)))
    (cond ((apply logic vals)
	(format outstream "~a;~a;~a;~a~%" index error-val message column)))))

(defun get-header-row (infile)
  "saca la fila header del csv infile"
  (let ((header nil))
    (with-open-file (stream infile)
      (dotimes (i 1)
	(setf header (cl-csv:read-csv (read-line stream nil :eof)))))
    (car header)))

(defun header-suite-works (header suite)
  "aproba si la fila header este representada dentro del suite. El valor
de retorno es una lista con los spec que funcionan"
  (let ((depends nil)
	(column nil)
	(to-check nil)
	(col-positions nil)
	(filtered (list)))
    (loop for spec in suite do
      (setf depends (getf spec :depends))
      (setf column (getf spec :column))
      (setf to-check (remove-duplicates (concatenate 'list (list column) depends)))
      (setf col-positions (loop for c in to-check
			 collect (get-col-position c header) into res
			 finally (return res)))
      (cond ((notany #'null col-positions)
	  (setf filtered (append filtered (list spec))))))
    filtered))


(defun run-validation (in outdir header suite worker-id)
  "funcion main"
  (with-open-file (stream in)
    (with-open-file (str (format nil "~aresult_~a.csv" outdir worker-id)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      ;; un bucle sobre las filas del csv.
      (loop for line = (read-line stream nil :eof) and idx from 0
	    until (eq line :eof) do

	      ;; hace que la fila header no esta processada
	      (let ((record (car (cl-csv:read-csv line))))
		(cond ((not (equal record header))

		       ;; por cada spec in suite, haz la validacion
		       (loop for spec in suite do
			 (let ((vals (get-values header spec record)))
			   (validate vals spec str idx))))))))))


(defparameter *suitetest*
  (list
   (list
    :column "vendor_id"
    :type "integer"
    :depends (list "vendor_id")
    :message "El valor tiene que ser un numero"
    :logic (lambda (count) (not (ignore-errors (parse-integer count)))))
   (list
    :column "id"
    :type "string"
    :depends (list "id")
    :message "El valor tiene empezar con id"
    :logic (lambda (id) (not (search "id" id))))
   (list
    :column "pickup_datetime"
    :type "date"
    :depends (list "pickup_datetime")
    :message "El valor tiene que ser una fecha como yyyy-mm-dd"
    :logic (lambda (x)
	     (not (local-time:parse-timestring x
					       :start 0
					       :end 10
					       :fail-on-error nil))))
   (list
    :column "passenger_count"
    :type "integer"
    :depends (list "passenger_count")
    :message "El valor tiene que ser un numero"
    :logic (lambda (count) (not (ignore-errors (parse-integer count)))))
   (list
    :column "passenger_count"
    :type "integer"
    :depends (list "passenger_count")
    :message "passenger count tiene que ser menos que 4"
    :logic (lambda (count) (cond ((not (not (ignore-errors (parse-integer count))))
				  (> (parse-integer count) 4)))))))
