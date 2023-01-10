(in-package :stream_validator)

(defun check-integer-string (x)
  (typep (parse-integer x :junk-allowed t) 'integer))

(defun check-date-string (x)
  (local-time:parse-timestring x
			       :start 0
			       :end 10
			       :fail-on-error nil))


(defparameter *taxidata*
  (list
   (list
    :column "vendor_id"
    :type "integer"
    :depends (list "vendor_id")
    :message "El valor tiene que ser un numero"
    :logic (symbol-function 'check-integer-string))
   (list
    :column "id"
    :type "string"
    :depends (list "id")
    :message "El valor tiene empezar con id"
    :logic (lambda (id) (search "id" id)))
   (list
    :column "pickup_datetime"
    :type "date"
    :depends (list "pickup_datetime")
    :message "El valor tiene que ser una fecha como yyyy-mm-dd"
    :logic (symbol-function 'check-date-string))
   (list
    :column "passenger_count"
    :type "integer"
    :depends (list "passenger_count")
    :message "El valor tiene que ser un numero"
    :logic (lambda (count) (typep (parse-integer count :junk-allowed t) 'integer)))
   (list
    :column "passenger_count"
    :type "integer"
    :depends (list "passenger_count")
    :message "passenger count tiene que ser menos que 4"
    :logic (lambda (count) (cond ((<= (parse-integer count :junk-allowed t) 4)))))))
