; asks if x is an atom
; returns boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) 
	 (not (null? x)))))

(define one?
  (lambda (x) (= x 1)))

(define sub1
  (lambda (x) (- x 1)))

(define add1
  (lambda (x) (+ x 1)))

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

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define sum-of-prefixes-b
  (lambda (preceeding tup)
    (cond
      ((null? tup) (quote ()))
      (else
	(cons (+ preceeding (car tup)) 
	      (sum-of-prefixes-b (+ preceeding (car tup)) (cdr tup)))
	)
      )
    )
  )

; The Eleventh Commandment:
; -------------------------
; Use additional arguments when a function needs to know what other arguments to the function have been like so far.

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else
	(pick (sub1 n) (cdr lat))
	)
      )
    )
  )

(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) (quote ()))
      (else
	(cons (pick (car tup)
		    (cons (car tup) rev-pre))
	      (scramble-b (cdr tup)
			  (cons (car tup) rev-pre)))
	)
      )
    )
  )

; Chapter 12: Take Cover

; `letrec' defines and returns a recursive function

(define multirember
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
	      (cond
		((null? lat) (quote ()))
		((eq? a (car lat))
		 (mr (cdr lat)))
		(else
		  (cons (car lat)
			(mr (cdr lat))))))))
	mr)
     lat)))

(define multirember2
  (lambda (a lat)
    (letrec
      ((mr (lambda (lat)
	     (cond
	       ((null? lat) (quote ()))
	       ((eq? a (car lat))
		(mr (cdr lat)))
	       (else
		 (cons (car lat)
		       (mr (cdr lat))))))))
      (mr lat))))

; The Twelfth Commandment
; -----------------------
; Use (letrec ...) to remove arguments that do not change for recursive applications

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else
	  (cons (car lat)
		((multirember-f test?) a (cdr lat)))
	  )
	)
      )
    )
  )

(define multirember-f2
  (lambda (test?)
    (letrec
      ((m-f
	 (lambda (a lat)
	   (cond
	     ((null? lat) (quote ()))
	     ((test? (car lat) a)
	      (m-f a (cdr lat)))
	     (else
	       (cons (car lat)
		     (m-f a (cdr lat))))))))
      m-f)))

(define member?
  (lambda (a lat)
    (letrec
      ((yes? (lambda (l)
	       (cond
		 ((null? l) #f)
		 ((eq? (car l) a) #t)
		 (else (yes? (cdr l)))))))
      (yes? lat))))

(define union
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
	     (cond
	       ((null? lat) #f)
	       ((eq? (car lat) a) #t)
	       (else
		 (M? a (cdr lat)))))))
      (U set1))))

; The Thirteenth Commandment
; --------------------------
; Use (letrec ...) to hide and protect functions.

(define union-letrec
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
	       (N? lat)))))
      (U set1))))

(define two-in-a-row-letrec?
  (letrec
    ((W (lambda (a lat)
	  (cond
	    ((null? lat) #f)
	    (else (or (eq? (car lat) a)
		      (W (car lat)
			 (cdr lat))))))))
    (lambda (lat)
	(W (car lat) (cdr lat)))))

(define sum-of-prefixes-letrec
  (letrec
    ((S (lambda (sss tup)
	  (cond
	    ((null? tup) (quote ()))
	    (else
	      (cons (+ sss (car tup))
		    (S (+ sss (car tup))
		       (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))

(define scramble-letrec
  (letrec
    ((P (lambda (tup rp)
	  (cond
	    ((null? tup) (quote ()))
	    (else (cons (pick (car tup)
			      (cons (car tup) rp))
			(P (cdr tup)
			   (cons (car tup) rp))))))))
    (lambda (tup)
      (P tup (quote ())))))

; Chapter 13: Hop, Skip, and Jump

(define intersect
  (lambda (set1 set2)
    (letrec
      ((I (lambda (set)
	    (cond
	      ((null? set) (quote ()))
	      ((member? (car set) set2)
	       (cons (car set)
		     (I (cdr set))))
	      (else
		(I (cdr set)))))))
      (I set1))))

(define intersectall
  (lambda (lset)
    (letrec
      ((A (lambda (lset)
	    (cond
	      ((null? (cdr lset))
	       (car lset))
	      (else (intersect (car lset)
			       (A (cdr lset))))))))
      (cond
	((null? lset) (quote ()))
	(else (A lset))))))


; I dunno... This looks horrible. If we're doing sanity checks why not just check if there's () in lset FIRST, along with the outer (cond)?

(define intersectall-ap
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else
                    (I (car lset)
                       (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                  ((J (lambda (s1)
                        (cond
                          ((null? s1) '())
                          ((member? (car s1) s2)
                           (cons (car s1) (J (cdr s1))))
                          (else
                            (J (cdr s1)))))))
                  (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
          (cond
            ((null? lset) '())
            (else (A lset))))))))

; The Fourteenth Commandment: 
; ---------------------------
; Use call-with-current-continuation to return values abruptly and promptly



