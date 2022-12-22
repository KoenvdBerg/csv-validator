(ql:quickload "cl-csv")

;; (defparameter *csv-cols*
;;   (car (cl-csv:read-csv #P"/home/koenvandenberg/insertdata/lisp/tst.csv")))

(defvar csv-cols 1)

(defparameter *suite*
  (list
   (list
    :column "order"
    :message "La columna es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "name"
    :message "el nombre tiene que consistir de palabras"
    :logic (lambda (x) (string= x "")))
   (list
    :column "city"
    :message "El valor Stockholm no es correcto"
    :logic (lambda (x) (string= x "Stockholm")))
   (list
    :column "article"
    :message "La columna article es vacio"
    :logic (lambda (x) (string= x "")))
   (list
    :column "size"
    :message "La columna size es vacio"
    :logic (lambda (x) (string= x "")))))

(defun get-col-position (col-name cols)
  "Saca la posicion (en numero) de una columna por su nombre"
  (position col-name cols :test #'string=))

(defun validate (value spec outfile index)
  (let ((message (getf spec :message))
	(column (getf spec :column))
	(logic (getf spec :logic)))
    (cond ((funcall logic value)
	(format outfile "~a;~a;~a;~a~%" index value message column)))))

(defun main (in out)
  "mueva lo que esta escrito abajo a una nueva funcion. progn --> pasos: 1 leer config, 2 validacion, 3 escribir resultados"
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
	      (let ((result (nth 0 (cl-csv:read-csv line))))
		(loop for spec in *suite* do
		  (let ((value (nth (get-col-position (getf spec :column)
						      (car csv-cols))
				    result)))
		    (validate value spec str idx)))))))))


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
