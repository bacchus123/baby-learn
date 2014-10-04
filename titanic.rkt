#lang racket
(require racket/runtime-path)
(require (planet neil/csv))
(require "decision-tree.rkt")


(define-runtime-path titanic-train "train.csv")
(define-runtime-path titanic-test "test.csv")
(define csv-reader (make-csv-reader-maker
			     '((separator-chars #\,)
			       (strip-leadingwhitespace? . #t)
			       (strip-trailing-whitespace? . #t))))
(define (get-rows reader)
  (let ((row (reader)))
    (define (loop)
      (let ((row (reader)))
	(if (null? row)
	    '()
	    (cons row (loop)))))   (loop)))


(define training-set  (get-rows (make-csv-reader (open-input-file titanic-train))))
(define test-set (get-rows  (make-csv-reader (open-input-file titanic-test))))

(define (survives sample) (list-ref sample 1))
(define (class sample) (list-ref sample 2))
(define (sex sample) (list-ref sample 4))
(define (age sample) (let ((age (string->number (list-ref sample 5))))
		       (cond ((eqv? age #f) -1)
			     ((< age 10) 1)
			     (else 0))))
(define (pass-id sample) (list-ref sample 0))
(define titanic-tree
  (make-tree survives (list class age sex) training-set))


(for-each (lambda (x) (begin  (write (car x))
			 (display  " , ")
			 (write  (cdr x))
			 (newline)))
     (map (lambda (x) (cons (pass-id x) (run-tree titanic-tree (cons '() x)))) test-set))

