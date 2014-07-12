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
  `(add-clause ',clause))

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

(defun prove (goal bindings)
  "Return a list of possible solutions to goal."
  (mapcan #'(lambda (clause)
	      (let ((new-clause (rename-variables clause)))
		(prove-all (clause-body new-clause)
			   (unify goal (clause-head new-clause) bindings))))
	  (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings fail) fail)
	((null goals) (list bindings))
	(t (mapcan #'(lambda (goal1-solution)
		       (prove-all (rest goals) goal1-solution))
		   (prove (first goals) bindings)))))

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

(defmacro ?- (&rest goals) `(top-level-prove ',goals))

(defun top-level-prove (goals)
 "Prove the goals, and print variables readably."
 (show-prolog-solutions
  (variables-in goals)
  (prove-all goals no-bindings)))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution))
	    solutions))
  (values))


(defun show-prolog-vars (vars bindings)
  "Print each variable with its bindings."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
	(format t "~&~a = ~a" var
		(subst-bindings bindings var))))
  (princ ":"))

    
