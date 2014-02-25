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

  
(defun add-debug (&rest ids)
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
  "Find a (variable . value) pair in a binding list. A binding is a list of
associated pair. Use extend bindings to create a new binding"
  (assoc var bindings))

(defun binding-var (binding)
  "Get the var part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding. See get-binding for detail."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun make-binding (var val)
  "Create a binding pair."
  (cons var val))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list. Use NO-BINDINGS for binding parameter
to create initial binding"
  (cons (make-binding var val)
	(if (eq bindings no-bindings) nil bindings)))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)
	 (single-matcher pattern input bindings))
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
  "Is this a segment matching pattern like ((?* var) . pat) ? Then the var
matches one or more element. For example '((?* ?p) need (?* ?x)) matches
'(Mr Hulot and I need a vacation) with ?P as '(MR HULOT AND I) and ?X
as (A VACATION)."
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       ; Check that it uses registerd syntaxes like ?* ?+
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern? E.g. (?is x predicate) (?and .patterns) (
?or . patterns)"
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
	   (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x, if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
where var-and-pred is the list (var pred).
Example, (match-is '(?is ?n numberp) 34 no-bindings) returns ((?N . 34))"
  (let* ((var (first var-and-pred))
        (pred (second var-and-pred))
	(new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
	    (not (funcall pred input)))
	fail
      new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input.
Example (match-and '((?is ?n numberp) (?is ?n oddp)) 3 no-bindings)
returns ((?N . 3))"
  (cond ((eq bindings fail) fail)
	((null patterns) bindings)
	(t (match-and (rest patterns) input
		      (pat-match (first patterns) input
				 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if all the patterns match the input. If multiple patterns
match the input, the returned binding is fromt the first pattern match.
Example: (match-or '(?x 2 3) 3 no-bindings) returns ((?X . 3))."
  (if (null patterns)
      fail
    (let ((new-bindings (pat-match (first patterns)
				    input bindings)))
      (if (eq new-bindings fail)
	  (match-or (rest patterns) input bindings)
	new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
This will never bind any variables.
Example: (match-not '((?is n oddp) 4)' 6 no-bindings) returns no-bindings"
  (if (match-or patterns input bindings)
      fail
    bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
    (pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
      ;; We assume that pat starts with a constant
      ;; In other words, a pattern can't have 2 consecutive vars
      (let ((pos (first-match-pos (first pat) input start)))
	(if (null pos)
	    fail
	  (let ((b2 (pat-match pat (subseq input pos)
			       (match-variable var (subseq input 0 pos) bindings))))
	    ;; If this match failed, try another longer one
	    ;; If it worked, check the variable match
	    (if (eq b2 fail)
		(segment-match pattern input bindings (+ pos 1))
	      b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
starting at position start. If pat1 is non-constant, then just
return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
	 (position pat1 input :start start :test #'equal))
	((< start (length input)) start)
	(t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "match zero or one element of input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
	(pat-match pat input bindings))))

;; Following may not work. see:
;; http://stackoverflow.com/questions/18331753
;; On function name and dynamic binding in common lisp
(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
The pattern looks like (?if code) . rest).
Example: 
  (match-if '((?if (eql (?op ?x ?y) ?z))) nil
'((?x . 3)(?op . +)(?y . 4)(?z 7))). Actually it may not work,
see comment above. But (match-if '((?if (eql (funcall ?op ?x ?y) ?z))) nil
'((?x . 3)(?op . +)(?y . 4)(?z 7)))"
  (and (progv (mapcar #'car bindings)
	   (mapcar #'cdr bindings)
	 (eval (second (first pattern))))
	 (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
	(expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
	((atom pat) pat)
	(t (cons (expand-pat-match-abbrev (first pat))
		 (expand-pat-match-abbrev (rest pat))))))
; Tests
(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))
; ?X matching a list
(pat-match '((?* ?X)) '(hello I am here))
(pat-match '(?X (?if (= ?x 3))) '(3))
(pat-match '((?is ?X numberp)) '(2))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun one-f (set)
  "Pick one element of set. and make a list of it."
  (list (random-elt set)))


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

;;(defun interactive-interpreter (prompt transformer)
;;  "Read an expression, transform it, and print the result."
;;  (loop
;;   (handle-case
;;    (progn
;;      (if (stringp prompt)
;;	  (print prompt)
;;	(funcall prompt)
;;	(print (funcall transformer (read)))))
;;    ;; In case of error, do this:
;;    (error (condition)
;;	   (format t "~&;; Error ~a ignored, back to top level."
;;		   condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompt like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states,
and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
	((funcall goal-p (first states)) (first states))
	(t (tree-search
	    (funcall combiner
		     (funcall successors (first states))
		     (rest states))
	    goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun bredth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun is (value) #'(lambda (x) (eql x value)))

(defun diff (num)
  "Returns the function that finds the difference from num."
  #'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun graph-search (states goal-p successors combiner &optional
		     (state= #'eql) old-states)
  "Find a state that satisfies goal-p. Start with states,
and search according to successors and combiner.
Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
	((funcall goal-p (first states)) (first states))
	(t (graph-search
	    (funcall
	     combiner
	     (new-states states successors state= old-states)
	     (rest states))
	    goal-p successors combiner state=
	    (adjoin (first states) old-states :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been before."
  (remove-if
   #'(lambda (state)
       (or (member state states :test state=)
	   (member state old-states :test state=)))
   (funcall successors (first states))))

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  #'(lambda (path) (funcall test value (funcall key path))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
	  (path-state path) (path-total-cost path)))


(defun a*-search (paths goal-p successors cost-fn cost-left-fn
		  &optional (state= #'eql) old-paths)
  "Find a path whose state satisfies goal-p, Start with paths,
 and expand successors, exploring least cost first.
 When there are duplicate states, keep the one with the 
 lower cost and discard the other."
  (dbg :search ";; Search: ~a old: ~a" paths old-paths)
  (cond
     ((null paths) fail)
     ((funcall goal-p (path-state (first paths)))
      (values (first paths) paths))
     (t (let* ((path (pop paths))
 	      (state (path-state path)))
 	 ;; Update PATHS and OLD-PATHS to reflect
 	 ;; the new succeors of STATE:
 	(setf old-paths (insert-path path old-paths))
	(dolist (state2 (funcall successors state))
	  (let* ((cost (+ (path-cost-so-far path)
 			   (funcall cost-fn state state2)))
 		  (cost2 (funcall cost-left-fn state2))
 		  (path2 (make-path
 			  :state state2 :previous path
 			  :cost-so-far cost
 			  :total-cost (+ cost cost2)))
 		  (old nil))
 		  ;; Place the new path, path2, in the right list:
 	  (cond
	    ((setf old (find-path state2 paths state=))
	     (when (better-path path2 old)
	       (setf paths (insert-path
			    path2 (delete old paths)))))
	    ((setf old (find-path state2 old-paths state=))
	     (when (better-path path2 old)
	       (setf paths (insert-path path2 paths))
	       (setf old-paths (delete old old-paths))))
	    (t (setf paths (insert-path path2 paths))))))
	   (a*-search paths goal-p successors cost-fn cost-left-fn
		      state= old-paths)))))


(defun find-path (state paths state=)
  "Find the path with this state among list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  ;; Merge is a built-in function
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (path-state path)
	    (path-states (path-previous path)))))


;; Shortest path a c e d f
(defparameter *example-graph* 
  '((a (b  4) (c  2)) (b (c 5) (d 10)) (c (e 3)) (d (f 11)) (e (d 4))))


(defun graph-successor (node)
  "Returns successors of a node."
  (mapcar #'car (graph-entry node)))

(defun graph-entry (node)
  "Returns graph entries of a node."
  (cdr (assoc node *example-graph*)))

(defun graph-cost (node1 node2)
  "Returns cost between node1 and node2."
  (second (assoc node2 (graph-entry node1))))

;; (path-states (a*-search (list (make-path :state 'a)) (is 'f) #'graph-successor
;; 		    #'graph-cost #'(lambda (x) 0)))

(defstruct (rule (:type list)) pattern response)

(defparameter
  *student-rules*
  (mapcar
   #'expand-pat-match-abbrev
   '(((?x* |.|) ?x)
     ((?x* |.| ?y*) (?x ?y))
     ((if ?x* |.| then ?y) (?x ?y))
     ((if ?x* |,| ?y*) (?x ?y))
     ((if ?x |,| ?y*) (?x ?y))
     ((?x* |,| and ?y*) (?x ?y))
     (((find ?x* and ?y*) ((= to-find-1 ?x) (= to-find-2 ?y))))
     ((find ?x*) (= to-find ?x))
     ((?x* equals y*) (= ?x ?y))
     ((?x* same as ?y*) (= ?x ?y))
     ((?x* = ?y*) (= ?x ?y))
     ((?x* is equal to ?y*) (= ?x ?y))
     ((?x* is ?y*) (= ?x ?y))
     ((?x* - ?y*) (- ?x ?y))
     ((?x* minus ?y*) (- ?x ?y))

     ((difference between ?x* and y*) (- ?y ?x))
     ((difference ?x* and ?y*) (- ?y ?x))
     ((?x* + ?y*) (+ ?x ?y))
     ((sum ?x* and ?y*) (+x ?x ?y))
     ((product ?x* and ?y*) (* ?x ?y))
     ((?x* * ?y*) (* ?x ?y))
     ((?x* times ?y*) (* ?x ?y))
     ((?x8 / ?y*) (/ ?x ?y))
     ((?x* per ?y*) (/ ?x ?y))
     ((?x* divided by ?y*) (/ ?x ?y))
     ((half ?x*) (/ ?x 2))
     ((one half ?x*) (/ ?x 2))
     ((twice ?x*) (* 2 ?x))
     ((square ?x*) (* ?x ?x))
     ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
     ((?x* % more than ?y*) (* ?y (/ (+ 1000 ?x) 100)))
     ((?x* % ?y*) (* (/ ?x 100) ?y)))))

(defun rule-based-translator
  (input rules &key (matcher #'pat-match)
	 (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in the rules that matches input. then apply
the action to that rule"
  (some
   #'(lambda (rule)
       (let ((result (funcall matcher (funcall rule-if rule) input)))
	 (if (not (eq result fail))
	     (funcall action result (funcall rule-then rule)))))
   rules))

;; Example (use-eliza-rules '(hello eliza))
(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (rule-based-translator input *eliza-rules*
			 :action #'(lambda (bindings responses)
				     (sublis (switch-viewpoint bindings)
					    (random-elt responses)))))
				      
(defun student (words)
  "Solve certain Algebra Word Problems."
  (solve-equations
   (create-list-of-equations
    (translate-to-expression (remove-if #'noise-word-p words)))))

(defun translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
       words *student-rules*
       :rule-if #'rule-pattern :rule-then #'rule-response
       :action #'(lambda (bindings response)
		   (sublis (mapcar #'translate-pair bindings)
			   response)))
      (make-variable words)))

(defun translate-pair (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
	(translate-to-expression (binding-val pair))))

(defun create-list-of-equations (exp)
  "Seperate out equations embedded in nested parens."
  (cond ((null exp) nil)
	((atom (first exp)) (list exp))
	(t (append (create-list-of-equations (first exp))
		   (create-list-of-equations (rest exp))))))

(defun make-variable (words)
  "Create a variable name based on the given list of words"
  ;; The list of words will already have noise words reoved
  (first words))

(defun noise-word-p (word)
  "Is this a low-content word that can be safely ignored?"
  (member word '(a an the this number of $)))

(defun solve-equations (equations)
  "Print the equations and their solution"
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations nil)))

(defstruct (expr (:type list) (:constructor mkexpr (lhs op rhs)))
		op lhs rhs)

(defun expr-p (x) (consp x))
(defun expr-args (x) (rest x))


(defun solve (equations known)
  "Solve a system of equations by constraint propagation."
  ;; Try to solve for one equation, and substitute its value into
  ;; the others. If that doesn't work, return what is known.
  (or (some #'(lambda (equation)
		(let ((x (one-unknown equation)))
		  (when x
		    (let ((answer (solve-arithmatic
				  (isolate equation x))))
		    (solve (subst (expr-rhs answer) (expr-lhs answer)
			   (remove equation equations))
		    (cons answer known))))))
	    equations)
      known))

(defun isolate (e x)
  "Isolate the lone x in e on the left-hand side of e."
  ;; This assumes there is exactly one x in e,
  ;; and that e is an equation.
  (cond ((eq (expr-lhs e) x)
	 ;; Case I: X = A -> X = n
	 e)
	((in-expr x (expr-rhs e))
	 ;; Case II: A = f(X) -> f(X) = A
	 (isolate (mkexpr (expr-rhs e) '= (expr-lhs e)) x))
	((in-expr x (expr-lhs (expr-lhs e)))
	 ;; Case III: f(X)*A = B -> f(X) = B/A
	 (isolate (mkexpr (expr-lhs (expr-lhs e)) '= 
			  (mkexpr (expr-rhs e)
				  (inverse-op (expr-op (expr-lhs e)))
				  (expr-rhs (expr-lhs e)))) x))
	((commutative-p (expr-op (expr-lhs e)))
	 ;; Case IV: A*f(X) = B -> f(X) = B/A
	 (isolate (mkexpr (expr-rhs (expr-lhs e)) '= 
			  (mkexpr (expr-rhs e)
				  (inverse-op (expr-op (expr-lhs e)))
				  (expr-lhs (expr-lhs e)))) x))
	(t ;; Case V: A/f(X) = B -> f(X) = A/B
	 (isolate (mkexpr (expr-rhs (expr-lhs e)) '=
			  (mkexpr (expr-lhs (expr-lhs e))
				  (expr-op (expr-lhs e))
				  (expr-rhs e))) x))))
(defun print-equations (header equations)
  "Print a list of equations."
  (format t "~%~a~{~% ~{ ~a~}~}~%" header
	  (mapcar #'prefix->infix equations)))

(defconstant operators-and-inverses
    '((+ -) (- +) (* /) (/ *) (= =)))

(defun inverse-op (op)
  (second (assoc op operators-and-inverses)))

(defun unknown-p (exp)
  (symbolp exp))

(defun in-expr (x exp)
  "True if x aprears anywhere in exp."
  (or (eq x exp)
      (and (expr-p exp)
	   (or (in-expr x (expr-lhs exp)) (in-expr x (expr-rhs exp))))))

(defun no-unknown (exp)
  "Returns true if there is no unknowns in exp."
  (cond ((unknown-p exp) nil)
	((atom exp) t)
	((no-unknown (expr-lhs exp)) (no-unknown (expr-rhs exp)))
	(t nil)))

(defun one-unknown (exp)
  "Returns the single unknown in exp. If is exactly one."
  (cond ((unknown-p exp) exp)
	((atom exp) nil)
	((no-unknown (expr-lhs exp)) (one-unknown (expr-rhs exp)))
	((no-unknown (expr-rhs exp)) (one-unknown (expr-lhs exp)))
	(t nil)))

(defun commutative-p (op)
  "Is operator commutative?"
  (member op '(+ * =)))

(defun solve-arithmatic (equation)
  "Do the arithmatic for the right hand side."
  ;; This assumes that the right-hand side is in the right form.
  (mkexpr (expr-lhs equation) '= (eval (expr-rhs equation))))

(defun binary-exp-p (x)
  (and (expr-p x) (= (length (expr-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
	      (if (binary-exp-p exp)
		  (list (expr-lhs exp) (expr-op exp) (expr-rhs exp))
		  exp))))


;; Tests 
(solve-equations '((= (+ 3 4) (* (- 5 (+ 2 x)) 7))
			    (= (+ (* 3 x) y) 12)))

(defun fib (n)
  "Compute the nth number in the Fibonacci sequene."
  (if (<= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun memo (fn name key test)
  "Return the memo-funcion of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	(multiple-value-bind (val found-p)
	    (gethash k table)
	  (if found-p
	      val
	      (setf (gethash k table) (apply fn args))))))))

(defun memoisze (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name) (memo (symbol-function fn-name) fn-name key test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo))
	(when table (clrhash table)))))

(defmacro defun-memo (fn args &body body)
  "Define a memoize function."
  '(memoize (defun ,fn ,args . ,body)))


(defun compile-rule (rule)
  "Translate a grammar rule into a LISp function definition."
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs) '(one-of `,rhs))
	      ((length=1 rhs) (build-code (first rhs)))
	      (t `(case (random ,(length rhs))
		    ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  "Returns a list of case-clauses"
  (when choices
    (cons (list number (list number (build-code (first choices))))
	  (build-cases (+ number 1) (rest choices)))))

(defun build-code (choice)
  "Append together multiple constituents"
  (cond ((null choice) nil)
	((atom choice) (list choice))
	((length=1 choice) choice)
	(t `(append ,@(mapcar #'build-code choice)))))

(defun length=1 (x)
  "Is X a list of length 1?"
  (and (consp x)(null (rest x))))


(defmacro defrule (&rest rule)
  "Define a grammar rule"
  (compile-rule rule))

(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :function #'(lambda () . ,body)))

(defun force (x)
  "Find the value of x, by computing if it is a delay."
  (if (not (delay-p x))
      x
    (progn
      (when (delay-function x)
	(setf (delay-value x)
	      (funcall (delay-function x)))
	(setf (delay-function x) nil))
      (delay-value x))))

(defmacro make-pipe (head tail)
  "Create a pipe by evaluating head and delaying tail."
  `(cons ,head (lambda () ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe) (first pipe))
(defun tail (pipe)
  "Return tail of pipe or list, and destructively update
the tail if it is a function."
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
    (rest pipe)))


(defun pipe-elt (pipe i)
  "The i-th element of a pipe, 0-based"
  (if (= i 0)
      (head pipe)
    (pipe-elt (tail pipe) (- i 1))))

(defun integers (&optional (start 0) end)
  "A pipe of integers from START to END.
If END is nil, this is an infinite pipe."
  (if (or (null end) (<= start end))
      (make-pipe start (integers (+ start 1) end))
    nil))

(defun enumerate (pipe &key count key (result pipe))
  "Go through all (or count) elemnets of pipe.
possibly applying the KEY function. (Try PRINT.)"
  ;; Returns RESULT, which defaults to the pipe itself.
  (if (or (eq pipe empty-pipe) (count 0))
	 result
	 (progn
	   (unless (null key) (funcall key (head pipe)))
	   (enumerate (tail pipe) :count (if count (- count 1))
		      :key key :result result))))

(defun filter (pred pipe)
  "Keep only items in pipe satisfying pred."
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
		 (filter pred (tail pipe)))
    (filter pred (tail pipe))))
