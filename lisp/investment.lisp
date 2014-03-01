
(defstruct (portfolio-values (:type list))
  symbol total-value)

(defparameter *my-portfolio* nil)

(defun get-random-sell (portfolio-values)
  "Create a roulette with a compartment for each stock symbol
and compartment size proportional to the value and throw a 
dart randomly to pick a sell candidate"
  (choose-element portfolio-values
		  (random (get-sum portfolio-values))))

(defun get-sum (portfolio-values)
  "Get sum of total portfolio values"
  (reduce #'+  portfolio-values :key #'second))

(defun choose-element (portfolio-values value)
  "Choose a element i so that sum(portfolio-values[0..i-1] <= 
value < sum(portfolio-values[0..i]"
  (let ((current (first portfolio-values)))
	(cond 
	 ((null portfolio-values) nil)
	 ((>= (second current) value) current)
	 (t (choose-element (cdr portfolio-values)
			   (- value (second current)))))))
