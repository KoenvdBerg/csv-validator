(ql:quickload "cl-csv")

(defparameter *suitetest*
  (list
   (list
    :column "order"
    :type "string"
    :depends (list "order" "name")
    :message "La columna es vacio"
    :logic (lambda (x0 x1) (and (string= x0 "") (string= x1 "Chris"))))
   (list
    :column "city"
    :type "string"
    :depends (list "city")
    :message "el nombre no puede ser Manchester"
    :logic (lambda (x) (string= x "Manchester")))
   (list
    :column "order"
    :type "string"
    :depends (list "order" "name" "city")
    :message "La columna tiene que ser llena si has Chris y Manchester"
    :logic (lambda (x0 x1 x2) (and (string= x0 "")
				   (and (string= x1 "Chris")
					(string= x2 "Manchester")))))))


(defun get-col-position (col-name cols)
  "Saca la posicion (en numero) de una columna por su nombre"
  (position col-name cols :test #'string=))

(defun get-values (csv-cols spec record)
  (let ((depends (getf spec :depends)))
    (loop for x in depends
	  collect (nth (get-col-position x (car csv-cols)) record)
	    into record-values
	  finally (return record-values))))
				    
(defun validate (vals spec outfile index)
  (let ((message (getf spec :message))
	(column (getf spec :column))
	(logic (getf spec :logic))
	(error-val (car vals)))
    (cond ((apply logic vals)
	(format outfile "~a;~a;~a;~a~%" index error-val message column)))))

(defun main (in out)
  (with-open-file (stream in)
    (with-open-file (str out
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format str "index;error_value;error_message;column~%")
      (loop for line = (read-line stream nil :eof) and idx from 0
	    if (eq idx 0)
	      collect (car (cl-csv:read-csv line)) into csv-cols
	    until (eq line :eof) do
	      (cond ((eq (mod idx 1000) 0)
		     (format t "#")))
	      (let ((record (nth 0 (cl-csv:read-csv line))))
		(loop for spec in *suitetest* do
		  (let ((vals (get-values csv-cols spec record)))
		    (validate vals spec str idx)))))))))




; "/home/koenvandenberg/insertdata/lisp/tst.csv"

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


(defparameter *suitewide*
  (list
   (list
    :column "1"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "2"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "3"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "4"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "5"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "6"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "7"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "8"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "9"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "10"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "11"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "12"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "13"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "14"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "15"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "16"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "17"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "18"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "19"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "20"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "21"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "22"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "23"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "24"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "25"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "26"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "27"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "28"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "29"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "30"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "31"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "32"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "33"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))))
