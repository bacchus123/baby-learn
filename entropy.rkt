#lang r6rs

(define (entropy Sample)
  (let ((len (length Sample)))
    (define (ent-iter s total seen-classes)
      (cond ((null? s) total)
	    ((member (class s) seen-classes)
	     (ent-iter (cdr s) total seen-classes))
	    (else (let ((ratio (/ (count Sample (class (car s))) len)))
		  (ent-iter
		   (cdr s)
		   (+ total (* -1 (* ratio (log2 ratio))))
		   (cons (class s) seen-classes))))))
    (ent-iter Sample 0 '())))

(define (count samples target)
  (foldl (lambda (x acc) (if (matches? x target)
			    (+ acc 1)
			    acc))
	0
	samples))


(define (matches? sample target)
  (eq? (class sample) target))
(define (class sample)
  (car sample))

(define (logn n base)
  (/ (log n) (log base)))
(define (log2 n)
  (logn n 2))
