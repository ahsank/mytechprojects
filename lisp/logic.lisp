(defconstant fail nil "Indicate pat-match failure")

(defconstant no-bindings '((t . t)) "Indicates pat-match success, with no variables.")

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

;; (get-binding '?x '((?y . b) (?x . a))) ==? (?X . A)
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

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (if (get-binding var bindings)
      (unify (lookup var bindings) x bindings)
    (extend-bindings var x bindings)))

    