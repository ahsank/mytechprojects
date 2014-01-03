;; Typing and experimenting with programs in book 
;; "Paradigms of AI programming: case studies in common lisp" by Peter Norvik

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convension."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)


(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds 
	    :add-list add-list :del-list del-list)))

(defvar *ops* nil "A list of available operatiors.")

(defstruct op "An operation"
	   (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (find-all-if #'action-p (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
			(setf current-state 
			      (achieve current-state g goal-stack)))
		    goals)
	     (subsetp goals current-state :test #'equal))
	current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds.
or if there is an appropriate op for that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
	((member-equal goal goal-stack) nil)
	(t (some #'(lambda (op) (apply-op state goal op goal-stack))
	   (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new. transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
			     (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action ~a" (op-action op))
      (append (remove-if #'(lambda (x)
			     (member-equal x (op-del-list op)))
			     state2)
			 (op-add-list op)))))

(defun find-all (item sequence &rest keyword-args
		      &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
	   :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "    " *debug-io*))
    (apply #'format *debug-io* format-string args)))

  
(defun adddebug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids. stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
		    (set-difference *dbg-ids* ids))))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose:
  ;; the number o operators.
  (length (setf *ops* (mapc 'convert-op oplist))))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   (make-op :action 'shop-install-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))

(defvar *home-state*
  '(son-at-home car-needs-battery have-money have-phone-book))

(defvar *school-state* '(son-at-school))

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
	(unless (equal a b)
	  (dolist (c blocks)
	    (unless (or (equal c a) (equal c b))
	      (push (move-op a b c) ops)))
	  (push (move-op a 'table b) ops)
	  (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
    `((,a on ,c) (space on ,b))))

(defconstant fail nil "Indicate pat-match failure")

(defconstant no-bindings '((t . t)) "Indicates pat-match success, with no variables.")

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val) bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-match pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		     (pat-match (first pattern) (first input) bindings)))
	(t fail)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))


(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

(defun segment-pattern-p (pattern)
  "Is his a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
    (pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
      ;; We assume that pat starts with a constant
      ;; In other words, a pattern can't have 2 consecutive vars
      (let ((pos (position (first pat) input :start start :test #'equal)))
	(if (null pos)
	    fail
	  (let ((b2 (pat-match pat (subseq input pos)
			       (match-variable var (subseq input 0 pos) bindings))))
	    ;; If this match failed, try another longer one
	    ;; If it worked, check the variable match
	    (if (eq b2 fail)
		(segment-match pattern input bindings (+ pos 1))
	      b2)))))))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y)) (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y))))

(defun eliza()
  "Respond to user input using pattern matching rules."
  (loop
   (print 'eliza>)
   (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
	    (let ((result (pat-match (rule-pattern rule) input)))
	      (if (not (eq result fail))
		  (sublis (switch-viewpoint result)
			  (random-elt (rule-responses rule)))))) *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa. and so on."
  (sublis '((I . you) (you . I) (me .you ) (am . are)) words))

(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it a list. otherwise (x)."
  (if (listp x) x (list x)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list of random."
  (elt choices (random (length choices))))

(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
   (handle-case
    (progn
      (if (stringp prompt)
	  (print prompt)
	(funcall prompt)
	(print (funcall transformer (read)))))
    ;; In case of error, do this:
    (error (condition)
	   (format t "~&;; Error ~a ignored, back to top level."
		   condition)))))
(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompt like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)
	 (single-matcher pattern input input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		     (pat-match (first pattern) (first input) bindings)))
	(t fail)))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segmenet-match) '(segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

