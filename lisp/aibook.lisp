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
