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

(define (class sample) (car sample))
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


(define (make-dec-tree classifier attributes examples) '())

(define test-examples (list (cons 't 'weak) (cons 't 'weak) (cons 't 'weak)
		   (cons 't 'weak) (cons 't 'weak) (cons 't 'weak)
		   (cons 't 'strong) (cons 't 'strong) (cons 't 'strong)
		   (cons 'f 'weak) (cons 'f 'weak) (cons 'f 'strong)
		   (cons 'f 'strong) (cons 'f 'strong)))

(define (wind-strength x) (cdr x))

(- (entropy class test) (gain wind-strength test))

(make-dec-tree class (list wind-strength) test-examples)
