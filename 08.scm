(load "helpers")

(define rember-f
  (lambda (test? s l)
    (cond ((null? l) l)
	  ((test? (car l) s) (cdr l))
	  (else (cons (car l)
		      (rember-f test? s (cdr l)))))))

(print (rember-f = 5 '(6 2 5 3)))
(print (rember-f eq? 'jelly '(jelly beans are good)))
(print (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(print (eq?-salad 'salad))
(print (eq?-salad 'tuna))
(print ((eq?-c 'salad) 'tuna))

(set! rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) l)
	    ((test? (car l) a) (cdr l))
	    (else (cons (car l)
			((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(print (rember-eq? 'tuna '(tuna salad is good)))
(print ((rember-f eq?) 'tuna '(tuna salad is good)))
(print ((rember-f eq?) 'tuna '(shirimp salad and tuna salad)))
(print ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) lat)
	    ((test? (car lat) old)
	       (cons new lat))
	    (else (cons (car lat)
			((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) lat)
	    ((test? (car lat) old)
	       (cons old
		     (cons new (cdr lat))))
	    (else (cons (car lat)
			((insertR-f test?) new old (cdr lat))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (test?)
      (lambda (new old l)
	(cond ((null? l) l)
	      ((test? (car l) old)
	         (seq new old (cdr l)))
	      (else (cons (car l)
			  (((insert-g seq) test?) new old (cdr l)))))))))

(print (((insert-g seqL) eq?) 'b 'c '(d c)))

(set! insertL-f (insert-g seqL))
(set! insertR-f (insert-g seqR))

(set! insertL-f
      (insert-g (lambda (new old l)
		  (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst ((insert-g seqS) eq?))

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    (((insert-g seqrem) eq?) #f a l)))
;; #f here plays the role of an ignored param
;; As it isn't useful for the the rember function


;; The Ninth Commandment
;;
;; Abstract common patterns with a new function.

(define atom-fo-function
  (lambda (x)
    (cond ((eq? x (quote +)) +)
	  ((eq? x (quote *)) *)
	  (else expt))))

(print (atom-fo-function (operator '(+ 5 3))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  (else ((atom-fo-function (operator nexp))
		   (value (1st-sub-exp nexp))
		   (value (2nd-sub-exp nexp)))))))

(print (value '(+ 5 3)))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) lat)
	    ((test? (car lat) a)
	       ((multirember-f test?) a (cdr lat)))
	    (else (cons (car lat)
			((multirember-f test?) a (cdr lat))))))))

(print ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) lat)
	  ((test? (car lat))
	     (multiremberT test? (cdr lat)))
	  (else (cons (car lat)
		      (multiremberT test? (cdr lat)))))))

(print (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col '() '()))
	  ((eq? (car lat) a)
	     (multirember&co a
			     (cdr lat)
			     (lambda (newlat seen)
			       (col newlat
				    (cons (car lat) seen)))))
	  (else (multirember&co a
				(cdr lat)
				(lambda (newlat seen)
				  (col (cons (car lat) newlat)
				       seen)))))))
;; this function splits the list in two: first part is a list of all
;; the values you want to keep. second part is a list of all the
;; values removed. using continuations.

(define a-friend
  (lambda (x y)
    (null? y)))

(print (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend))
(print (multirember&co 'tuna '() a-friend))
(print (multirember&co 'tuna '(tuna) a-friend))
(print (multirember&co 'tuna '(and tuna) a-friend))

(define last-friend
  (lambda (x y)
    (length x)))

(print (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))


;; The Tenth Commandment
;;
;; Build functions to collect more than one value at a time.

(define multiinsertLR-v1
  (lambda (new oldL oldR lat)
    (multiinsertR new oldR (multiinsertL new oldL lat))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) oldL)
	     (cons new
		   (cons oldL
			 (multiinsertLR new oldL oldR (cdr lat)))))
	  ((eq? (car lat) oldR)
	     (cons oldR
		   (cons new
			 (multiinsertLR new oldL oldR (cdr lat)))))
	  (else (cons (car lat)
		      (multiinsertLR new oldL oldR (cdr lat)))))))

(print (multiinsertLR 'tuna 'chips 'cheese '(chips and cheese or cheese and chips)))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
	  ((eq? (car lat) oldL)
	     (multiinsertLR&co new oldL oldR (cdr lat)
			       (lambda (newlat L R)
				 (col (cons new (cons oldL newlat))
				      (+ 1 L)
				      R))))
	  ((eq? (car lat) oldR)
	     (multiinsertLR&co new oldL oldR (cdr lat)
			       (lambda (newlat L R)
				 (col (cons oldR (cons new newlat))
				      L
				      (+ 1 R)))))
	  (else (multiinsertLR&co new oldL oldR (cdr lat)
				  (lambda (newlat L R)
				    (col (cons (car lat) newlat)
					 L
					 R)))))))

(print (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
			 (lambda (x y z)
			   x)))
(print (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
			 (lambda (x y z)
			   y)))
(print (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
			 (lambda (x y z)
			   z)))

(define evens-only*
  (lambda (l)
    (cond ((null? l) l)
	  ((atom? (car l))
	     (cond ((even? (car l))
		    (cons (car l)
			  (evens-only* (cdr l))))
		   (else (evens-only* (cdr l)))))
	  (else (cons (evens-only* (car l))
		      (evens-only* (cdr l)))))))

(print (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
	  ((atom? (car l))
	     (cond ((even? (car l))
		      (evens-only*&co (cdr l)
				      (lambda (newL e o)
					(col (cons (car l) newL)
					     (* e (car l))
					     o))))
		   (else (evens-only*&co (cdr l)
					 (lambda (newL e o)
					   (col newL
						e
						(+ o (car l))))))))
	  (else (evens-only*&co (car l)
				(lambda (carL carE carO)
				  (evens-only*&co (cdr l)
						  (lambda (cdrL cdrE cdrO)
						    (col (cons carL cdrL)
							 (* carE cdrE)
							 (+ carO cdrO))))))))))

(print (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
		       (lambda (newl prod sum)
			 (cons sum
			       (cons prod newl)))))
