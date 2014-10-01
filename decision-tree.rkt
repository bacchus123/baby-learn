#lang racket

(define (entropy accessor samples)
  (let ((len (length samples)))
    (define (ent-iter s total seen-classes)
      (cond ((null? s) total)
	    ((member (car (accessor s)) seen-classes)
	     (ent-iter (cdr s) total seen-classes))
	    (else (let ((ratio (/ (count accessor s (car (accessor s))) len)))
		  (ent-iter
		   (cdr s)
		   (+ total (* -1 (* ratio (log2 ratio))))
		   (cons (car (accessor s)) seen-classes))))))
    (ent-iter samples 0 '())))

(define (count accessor samples target)
  (foldl 
   (lambda (x acc) 
     (if (eq? (accessor x) target)
         (+ acc 1)
         acc))
   0
   samples))

(define (logn n base)
  (/ (log n) (log base)))
(define (log2 n)
  (logn n 2))

(define (samples-with-attrib-val  value accessor samples)
  (filter (lambda (x) (eq? (accessor x) value)) samples))

(define (gain attribute examples)
  (let ((len (length examples)))
    (define (gain-loop examples total seen-values)
      (cond ((null? examples) total)
	    ((member (attribute (car examples)) seen-values) (gain-loop (cdr examples) total seen-values))
	    (else (let ((with-this-value (samples-with-attrib-val
					 (attribute (car examples))
					 attribute
					 examples)))
		    (gain-loop
		     (cdr examples)
		     (+ total
			(* (/ (length with-this-value) len)
			   (entropy class with-this-value))) (cons (attribute (car examples)) seen-values) )))))
    (gain-loop examples 0 '())))


(define (all-positive classes)
  (all-the-same #t classes))
(define (all-negative classes)
  (all-the-same #f classes))

(define (all-the-same value list)
  (if (null? list)
      #t
      (if (not (eq? (car list) value))
	  #f
	  (all-the-same (cdr list) value))))

(define (most-common-value classes)
  (define (greatest-number h)
    (foldl (lambda (p a) (if (< (cdr p) (cdr a)) a p))
	   (cons '() 0)
	   (hash->list h)))
  (define (collect-values hash values)
    (if (null? values)
	hash
	(collect-values
	 (hash-update hash (car values) (lambda (x) (+ x 1)) 0)
	 (cdr values))))
  (greatest-number (collect-values (make-immutable-hash) classes)))

(define (best-classifies attributes examples)
  (foldl (lambda (attrib acc)
	  (let ((gains (gain attrib examples)))
	    (if (> gains (car acc))
		(cons gains attrib)
		acc)))
         (cons -1 '()) attributes))


(define (list-values attribute examples)
  (foldl (lambda (x acc)
	   (if (not (member (attribute x) acc))
	       (cons (attribute x) acc)
	       acc))'() examples))

(define (filter-on-attribute attribute examples)
  (define (filter-iter hash examples)
    (if (null? examples) hash
	(let ((value (attribute (car examples))))
	  (filter-iter
	   (begin (hash-set! hash value (cons (car examples) (hash-ref! hash value '()))) hash)
	   (cdr examples)))))
  (filter-iter (make-hash) examples))

(define (make-tree classifier attributes examples)
  (define (tree-itr a e)
    (let ((c (map classifier e)))
      (define (make-node attrib values)
	(define (node-itr val-pair)
	  (cond ((null? val-pair) '())
		((null? (cdar val-pair)) (most-common-value c))
		(else (cons (cons (caar val-pair)
				  (tree-itr (remove attrib a) (cdar val-pair)))
			    (node-itr (cdr val-pair))))))
	(cons attrib  (node-itr values)))
      (cond ((all-positive c) (car c))
	    ((all-negative c) (car c))
	    ((null? a) (car  (most-common-value c)))
	    (else (let ((best-a (cdr (best-classifies a e))))
		    (make-node best-a  (hash->list (filter-on-attribute best-a e))))))))
  (tree-itr attributes examples))

(define (run-tree tree sample)
  (define (run-itr tree)
    (define (test-value attrib values)
      (cond ((null? values)
	     (error "sample doesn't match any of the values... u goofed" tree sample))
	    ((eq? (caar values) (attrib sample))
	     (run-itr (cdar values)))
	    (else (test-value attrib (cdr values))))) 
    (cond ((null? tree) (error "Whoops we didn't end up classifying this..."))
	  ((pair? tree) (test-value (car tree) (cdr tree)))
	  (else tree)))
  (run-itr tree))

(define test-examples (list (cons 't 'weak) (cons 't 'medium) (cons 't 'weak)
			    (cons 't 'weak) (cons 't 'medium) (cons 't 'weak)
			    (cons 't 'weak) (cons 't 'weak) (cons 't 'weak)
			    (cons 'f 'strong) (cons 'f 'strong) (cons 'f 'strong)
			    (cons 'f 'medium) (cons 'f 'medium)))

(define (class sample) (car sample))
(define (wind-strength x) (cdr x))

(define new-tree (make-tree class (list wind-strength) test-examples))
(run-tree new-tree (cons '() 'weak))
