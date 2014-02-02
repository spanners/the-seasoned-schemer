(define union-letwrecked
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
	    (cond
	      ((null? set) set2)
	      ((M? (car set) set2)
	       (U (cdr set)))
	      (else (cons (car set)
			  (U (cdr set)))))))
       (M? (lambda (a lat)
	     (letrec
	       ((N? (lambda (lat)
		      (cond
	                ((null? lat) #f)
	                ((eq? (car lat) a) #t)
	                (else
			  (N? (cdr lat)))))))
	       (N? lat))))
      (U set1)))))
