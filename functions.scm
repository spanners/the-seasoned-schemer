; asks if x is an atom
; returns boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) 
	 (not (null? x)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
	(two-in-a-row-b? (car lat) (cdr lat))
	)
      )
    )
  )

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
	(or (eq? a (car lat))
	    (two-in-a-row? (cdr lat)))
	)
      )
    )
  )

(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond
      ((null? lat) #f)
      (else
	(or (eq? (car lat) preceeding)
	    (two-in-a-row-b? (car lat) (cdr lat)))
	)
      )
    )
  )
