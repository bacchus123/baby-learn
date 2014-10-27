(modeule linear
	 racket
	 (define (mean-square e x y)
	   (square (- (e x) y)))
	 (define (mean-squares e s f c)
	   (let ((l (length s)))
	     (define (mean-squares-sum e x y err)
	       (cond ((and (null? x) (null? y)) err)
		     (else (mean-squares-sum e (cdr x) (cdr y)
					     (+ err (mean-square e (car x) (car y))))))))
	     (* (/ 1 (* 2 l)) (mean-squares-sum e (map f s) (map c s) 0))))
	 (define (gradient-decent) ())

	 )
