(defconstant fail nil "Indicate pat-match failure")

(defconstant no-bindings '((t . t)) "Indicates pat-match success, with no variables.")

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

;; (get-binding '?x '((?y . b) (?x . a))) == (?X . A)
(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list. A binding is a list of
associated pair. Use extend bindings to create a new binding"
  (assoc var bindings))

;; (binding-var '(?X . A)) ==> ?X
(defun binding-var (binding)
  "Get the var part of a single binding."
  (car binding))

;; (binding-val '(?X . A)) ==> A
(defun binding-val (binding)
  "Get the value part of a single binding. See get-binding for detail."
  (cdr binding))

;; (lookup '?X '((?Y . B) (?X . A))) ==> A
(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

;; (make-binding '?X 'A) ==> (?X . A)
(defun make-binding (var val)
  "Create a binding pair."
  (cons var val))

;; (extend-binding '?X 'A '((?Y . B))) 
;; ==> ((?X . A) (?Y . B)
(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list. Use NO-BINDINGS for binding parameter
to create initial binding"
  (cons (make-binding var val)
	(if (eq bindings no-bindings) nil bindings)))

;; (match-variable '?X 'A '((?X . A) (?Y . B))) ==> ((?X . A) (?Y . B))
;; (match-variable '?X 'C '((?X . A) (?Y . B))) ==> nil
;; (match-variable '?Y 'B '((?X . A))) ==> ((?X . A) (?Y . B))
(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	 (t fail)))

;; (unify-variable '?x 'a '((?y . b) (?x . a))) => '((?Y . B) (?X . A))
;; (unify-variable '?x 'a '((?y . a ) (?x . b))) ==> NIL
;; (unify-variable '?x '?y '((?y . a) (?x . a))) ==> '((?Y . A) (?X . A))
;; (unify-variable '?x '?y '((?y . a) (?x . b))) ==> NIL
;; (unify-variable '?x '?y '((?y . a))) ==>  '((?X . A) (?Y . A))
;; (unify-variable '?x '?y '((?x . a))) ==> '((?Y . A) (?X . A))
;; (unify-variable '?x '?y '((?x . ?y)) ==> '((?X . ?Y))
;; (unify-variable '?x 'a '((?x . ?y) (?y . a))) ==> ((?X . ?Y) (?Y . A))
(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((and *occurs-check* (occurs-check var x bindings)) fail)
	(t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

;;  (subst-bindings '((?x . a)) '?x) ==> A
;;  ((subst-bindings '((?x . a) (?y . ?x)) '?y) ==> A
(defun subst-bindings (bindings x)
  "Substitute the value of variable in bindings into x,
taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
	((eq bindings no-bindings) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x))
		       x))))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun unifier (x y)
  "Returns something that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))

;; Prolog : population(sf, 750000). ==> (population SF 750000)
;; Clauses are representd as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; Clauses are on the predicate's property list
;; 
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))
(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")
;; I could have just taken code from http://norvig.com/paip?

;; (<- (member ?item (?x . ?rest)) (member ?item ?rest))
;; Here predicate is member
;; Prolog: member(X, [Y|L]) :- member(X, L).
(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))

(defmacro ?- (&rest goals)
  "Make a query and print answers."
  `(top-level-prove ',(replace-?-vars goals)))

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicat."
  (setf (get predicate 'clauses) nil))

(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
	((null goals) bindings)
	(t (prove (first goals) bindings (rest goals)))))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
	(some 
	 #'(lambda (clause)
	     (let ((new-clause (rename-variables clause)))
	       (prove-all
		(append (clause-body new-clause) other-goals)
		(unify goal (clause-head new-clause) bindings))))
	      clauses)
      ;; The predicate's "clauses" can be an atom:
      ;; a primitive function to call
      (funcall clauses (rest goal) bindings
	       other-goals))))

(setf (get 'show-prolog-vars 'clauses) nil)
(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x))
	  x))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree
					  &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
with duplication removed."
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	found-so-far)
    (unique-find-anywhere-if
     predicate
     (first tree)
     (unique-find-anywhere-if  predicate (rest tree)
			      found-so-far))))

(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
	     no-bindings)
  (format t "~&No.")
  (values))

(defun show-prolog-vars (vars bindings other-goals)
  "Print the variables with its-bindings."
  (if (null vars)
      (format t "~&Yes")
    (dolist (var vars)
      (format t "~&~a = ~a" var
	      (subst-bindings bindings var))))
  (if (continue-p)
      fail
    (prove-all other-goals bindings)))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
     (format t " Type ; to see more or . to stop")
     (continue-p))))

(clear-predicate 'zebra)
(clear-predicate 'iright)
(clear-predicate 'nextto)
(clear-predicate '=)

(<- (zebra ?h ?w ?z)
    ;; Each house is of the form:
    ;; (house nationality pet cigarette drink house-color)
    (= ?h ((house norwegian ? ? ? ?)
	   ?
	   (house ? ? ? milk ?) ? ?))
    (member (house englishman ? ? ? red) ?h)
    (member (house spaniard dog ? ? ?) ?h)
    (member (house ? ? ? coffee green) ?h)
    (member (house ukrainian ? ? tea ?) ?h)
    (iright (house ? ? ? ? ivory)
	    (house ? ? ? ? green) ?h)
    (member (house ? snails winston ? ?) ?h)
    (member (house ? ? kools ? yellow) ?h)
    (nextto (house ? ? chesterfield ? ?)
	    (house ? fox ? ? ?) ?h)
    (nextto (house ? ? kools ? ?)
	    (house ? horse ? ? ?) ?h)
    (member (house ? ? luckystrike orange-juice ?) ?h)
    (member (house japanese ? parliaments ? ?) ?h)
    (nextto (house norwegian ? ? ? ?)
	    (house ? ? ? ? blue) ?h)
    ;; Now for the question
    (member (house ?w ? ? water ?) ?h)
    (member (house ?z zebra ? ? ?) ?h))

(clear-predicate 'member)
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
    (iright ?left ?right ?rest))

(<- (= ?x ?x))


(defconstant unbound "Unbound")
(defstruct (var (:constructor ? ())
		(:print-function print-var))
	   (name (incf *var-counter*))
	   (binding unbound))

(defun print-var (var stream depth)
  (if (or (and (number p *print-level*)
	       (>= depth *print-level*))
	  (var-p (deref var)))
      (format stream "?~a" (var-name var))
    (write var :stream stream)))

(defun bound-p (var) (not (eq (var-binding var) unbound)))

(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
		do (setf ,exp (var-binding ,exp)))
	  ,exp))

(defun unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (deref x) (deref y)) t)
	((var-p x) (set-binding! x y))
	((var-p y) (set-bidnng! y x))
	((and (consp x) (consp y))
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))

(defun set-binding! (var value)
  "Set var's binding to value, after saving teh variable
in the trail. Always succeeds (return t)."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
	do (setf (var-binding (vector-pop *trail*)) unbound)))

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0)


(defun prolog-compile (symbol &optional
			      (clauses (get-clauses symbol)))
  "Compile a symbol: make a separate function for each arity."
  (unless (null claues)
    (let ((arity (relation-arity (claue-head (first claueses)))))
	  ;; Compile the clauses with this arity
	  (compile-predicate
	   symbol airity (clauses-with-arity clauses #'=arity))
	  ;; Compile all the clauses with any other arity
	  (prolog-compile
	   symbol (claues-with-arity clauses #'/= arity)))))

(defun claues-wth-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
	    :key #'(lambda(clause)
		     (relation-arity (clause-head clause)))
	    :test test))

(defun relation-arity (relation)
  "The number arguments to a relation.
Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation" (rest x))

(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
into a single LISP function."
  (let ((predicate (make-predicate symbol arity))
	(parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
	 .,(maybe-add-undo-bindings
	    (mapcar #'(lambda (clause)
			  (compile-clause parameter clause 'cont))
		      clauses)))))))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
	collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Returm the symbol: symbol/arity"
  (symbol symbol '/ arity))

(defun compile-clause (params clause cont)
  "Transform away the head, then compile the resulting body."
  (bind-unbound-vars
   params
   (compile-body
    (nconc
     (mapcar #'make-= params (args (clause-head clause)))
     (clause-body clause))
    cont)))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
    `((let ((old-trail (fill-pointer *trail*)))
	,(first compiled-exps)
	,@(loop for exp in (rest compiled-exps)
		collect '(undo-bindings old-trail)
		collect exp)))))

(defun bind-unbound-vars (parameer exp)
  "If there are any variables in exp (besides the parameters)
then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
				  parameters)))
    If (exp-vars
	`(let ,(mapcar #'(lambda (var) `(,var (?))) ;;** why ` before var?
		       exp-vars)
	   ,exp)
	exp)))

(defun make-= (x y) `(= ,x ,y))

(defun compile-body (body cont)
  "Compile the body of a cause."
  (if (null body)
      `(funcall ,cont)
    (let* ((goal (first body))
	   (macro (prolog-compiler-macro (predicate goal)))
	   (macro-val (if macro
			  (funcall macro goal (rest body) cont))))
      (if (and macro (not (eq macro-val :pass)))
	  macro-val
	(compile-call
	 (make-predicate (predicate goal)
			 (relation-arity goal))
	 (mapcar #'(lambda (arg) (compile-arg arg))
		 (args goal))
	 (if (null (rest body))
	     cont
	   `#'(lambda()
		,(compile-body (rest body) cont))))))))

(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro fo a Prolog predicate."
  ;; Note NAME is the raw name, not the same/arity
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
	 #'(lambda ,arglist .,body)))

(def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
	:pass
      `(if ,(compile-unify (first args) (second args))
	   ,(compile-body body cont)))))

(defun compile-unify (x y)
  "Return code that tests if var and term unify."
  `(unify! ,(compile-arg x) ,(compile-arg y)))

(defun compile-arg (arg)
  "Generate code for an argument to goal in the body."
  (cond ((variable-p arg) arg)
	((not (has-variable-p arg)) `',arg)
	((proper-listp arg)
	 `(list .,(mapcar #'compile-arg arg)))
	(t `(cons ,(compile-arg (first arg))
		  ,(compile-arg (rest arg))))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

	   

