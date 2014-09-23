#lang racket

(define (entropy Sample)
  (let ((len (length Sample)))
    (define (ent-iter s total seen-classes)
      (cond ((null? s) total)
	    ((member (car s) seen-classes)
	     (ent-iter (cdr s) total seen-classes))
	    (else (let ((ratio (/ (count s (car s)) len)))
		  (ent-iter
		   (cdr s)
		   (+ total (* -1 (* ratio (log2 ratio))))
		   (cons (car s) seen-classes))))))
    (ent-iter Sample 0 '())))

(define (count samples target)
  (foldl 
   (lambda (x acc) 
     (if (eq? x target)
         (+ acc 1)
         acc))
   0
   samples))

(define (logn n base)
  (/ (log n) (log base)))
(define (log2 n)
  (logn n 2))


(define (make-dec-tree samples attributes) '())

;;gain for an attribute value is  for_each value in attribute { |with_value| /|examples| * Entropy(with_value)}
(define (gain examples attribute)
  (let ((total (length examples))
        (values (map attribute examples)))
    (define (gain-iter examples total seen-values) 
      (cond ((null? examples) total)
            ((member (car examples) seen-values)
             (gain-iter (cdr examples) total seen-values))
            (else (let (this-value (filter (lambda (x) (eq? x (car examples))) examples))
                    ((gain-iter (cdr examples)
                             (+ total (* (/ (length this-value) total) (entropy (filter ) ))))))
    (gain-iter values 0 '())))
