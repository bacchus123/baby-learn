(module titanic-model racket
  (provide titanic-tree)
  (require racket/runtime-path)
  (require (planet neil/csv))
  (require "decision-tree.rkt")
  
  (module+ main
    (run-model))
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
	      (cons row (loop)))))  (loop)))


  (define training-set  (get-rows (make-csv-reader (open-input-file titanic-train))))
  (define test-set (get-rows  (make-csv-reader (open-input-file titanic-test))))

  (define (survives sample) (list-ref sample 1))
  (define (class sample) (list-ref sample 2))
  (define (sex sample) (list-ref sample 4))
  (define (age sample) (let ((age (string->number (list-ref sample 5))))
			 (cond ((eqv? age #f) 0)
			       ((< age 18) 1)
			       (else 0))))

  (define (sib sample) (let ((sib (string->number (list-ref sample 6))))
			 (if (> sib 7) 7 sib)))
  (define (pass-id sample) (list-ref sample 0))

  (define (fare sample) (let ((fare (string->number (list-ref sample 9))))
			  (cond ((eq? fare #f) 0)
				((< fare 10) 1)
				((< fare 20) 2)
				((< fare 30) 3)
				(else 4))))

  (define (embarked sample) (list-ref sample) 11)
  (define titanic-tree
    (make-tree survives (list class sex fare) training-set))
  (define (run-model) (for-each (lambda (x)
				  (begin  (display (car x))
					  (display  ",")
					  (display (cdr x))
					  (newline)))
				(map (lambda (x) (cons (pass-id x) (run-tree titanic-tree (cons '() x)))) test-set))))
