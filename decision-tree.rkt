#lang racket
(provide make-tree run-tree)
(define (entropy accessor samples)
  (let ((len (length samples)))
    (define (ent-iter s total seen-classes)
      (cond ((null? s) total)
	    ((member (accessor (car s)) seen-classes)
	     (ent-iter (cdr s) total seen-classes))
	    (else (let ((ratio (/ (count accessor s (accessor (car s))) len)))
		    (ent-iter
		     (cdr s)
		     (+ total (* -1 (* ratio (log2 ratio))))
		     (cons (accessor (car s)) seen-classes))))))
    (ent-iter samples 0 '())))

(define (count accessor samples target)
  (foldl 
   (lambda (x acc) 
     (if (equal? (accessor x) target)
	 (+ acc 1)
	 acc))
   0
   samples))

(define (logn n base)
  (/ (log n) (log base)))
(define (log2 n)
  (logn n 2))

(define (samples-with-attrib-val  value accessor samples)
  (filter (lambda (x) (equal? (accessor x) value)) samples))

(define (gain attribute classifier examples)
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
			   (entropy classifier with-this-value))) (cons (attribute (car examples)) seen-values) )))))
    (gain-loop examples 0 '())))

(define (all-the-same list)
  (foldl (lambda (x acc)
	   (if (equal? (cdr acc) x)
	       acc
	       (cons #f '()))) (cons #t (car list)) (cdr list)))

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

(define (best-classifies attributes class examples)
  (foldl (lambda (attrib acc)
	   (let ((gains (gain attrib class examples)))
	     (if (< gains (car acc))
		 (cons gains attrib)
		 acc)))
	 (cons (gain (car attributes) class examples) (car attributes)) (cdr attributes)))


(define (list-values attribute examples)
  (foldl (lambda (x acc)
	   (if (not (member (attribute x) acc))
	       (cons (attribute x) acc)
	       acc))'() examples))

(define (filter-on-attribute attribute examples)
  (foldl (lambda (sample acc)
	   (hash-update acc (attribute sample) (lambda (x) (cons sample x)) '()))
	 (make-immutable-hash)
	 examples))

(define (make-tree classifier attributes examples)
  (define (tree-itr a e)
    (let ((c (map classifier e)))
      (define (make-node attrib values)
	(define (node-itr val-pair)
	  (cond ((null? val-pair) (cons '() (car (most-common-value c))))
		((null? (cdar val-pair)) (car  (most-common-value c)))
		(else (cons (cons (caar val-pair)
				  (tree-itr (remove attrib a) (cdar val-pair)))
			    (node-itr (cdr val-pair))))))
	(cons attrib (node-itr values)))
      (let ((same (all-the-same c)))
	(cond ((car same) (cdr same))
	      ((null? a) (car  (most-common-value c)))
	      (else (let ((best-a (cdr (best-classifies a classifier  e))))
		      (make-node best-a  (hash->list (filter-on-attribute best-a e)))))))))
  (tree-itr attributes examples))

(define (run-tree tree sample)
  (define (run-itr tree)
    (define (test-value attrib values)
      (cond ((equal? (car  values) '())
	     (cdr values))
	    ((equal? (caar values) (attrib sample))
	     (run-itr (cdar values)))
	    (else (test-value attrib (cdr values))))) 
    (cond ((null? tree) (error "Whoops we didn't end up classifying this..."))
	  ((pair? tree) (test-value (car tree) (cdr tree)))
	  (else tree)))
  (run-itr tree))

;; (define test-examples (list (list 'sunny 'hot 'high 'weak 'f)
;; 			    (list 'sunny 'hot 'high 'strong 'f)
;; 			    (list 'overcast 'hot 'high 'weak 't)
;; 			    (list 'rain 'mild 'high 'weak 't)
;; 			    (list 'rain 'cool 'normal 'weak 't)
;; 			    (list 'rain 'cool 'normal 'strong 'f)
;; 			    (list 'overcast 'cool 'normal 'strong 't)
;; 			    (list 'sunny 'mild 'high 'weak 'f)
;; 			    (list 'sunny 'cool 'normal 'weak 't)
;; 			    (list 'rain 'mild 'normal 'weak 't)
;; 			    (list 'sunny 'mild 'normal 'strong 't)
;; 			    (list 'overcast 'mild 'high 'strong 't)
;; 			    (list 'overcast 'hot 'normal 'weak 't)
;; 			    (list 'rain 'mild 'high 'strong 'f)))

;; (define (classifier sample) (list-ref sample 4))
;; (define (wind-strength x) (list-ref x 3))
;; (define (humidity x) (list-ref x 2))
;; (define (temperature x) (list-ref x 1))
;; (define (outlook x) (list-ref x 0))



;; (define new-tree (make-tree classifier (list wind-strength humidity  temperature outlook) test-examples)

  
