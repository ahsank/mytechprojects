(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convension."
  (unless (some #'executing-op (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)


(defun op (action &key preconds add-list del-lists)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds 
	    :add-list add-list :del-list del-list)))
