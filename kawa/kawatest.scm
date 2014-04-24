;; Soring
(define-simple-class <comparator> (<java.util.Comparator>)
  (function init-keyword: function:)
  ((compare (o1 :: <java.lang.Object>) (o2 :: <java.lang.Object>)) :: <int>
   ((slot-ref (this) 'function) o1 o2)))

(define (sort function list)
  (let ((comparator (make <comparator> function: function)))
	(invoke-static <java.util.Collections> 'sort list comparator)
	list))

;; Test
(define (comp x1 x2) (cond ((< x1 x2) -1) ((> x1 x2) 1) (else 0)))

(define lu (vector 12 8 7 44 -1))

(define ls (sort comp lu))


(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
	  (begin
	    (close-input-port p)
	    (list->string (reverse ls1)))
	  (loop (cons c ls1) (read-char p))))))