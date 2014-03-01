(load "helpers")

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a s-or-n lat)
    (cond ((number? s-or-n) (keep-looking a (pick s-or-n lat) lat))
	  (else (eq? s-or-n a)))))

(define pick
  (lambda (n l)
    (cond ((zero? (- n 1)) (car l))
	  (else (pick (- n 1) (cdr l))))))

(print (pick 6 '(6 2 4 caviar 5 7 3)))
(print (pick 7 '(6 2 4 caviar 5 7 3)))

(print (looking 'caviar '(6 2 4 caviar 5 7 3)))
(print (looking 'caviar '(6 2 grits caviar 5 7 3)))
;(print (looking 'caviar '(7 1 2 caviar 5 6 3)))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))

(print (shift '((a b) c)))
(print (shift '((a b) (c d))))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
	  ((pair? (first pora)) (align (shift pora)))
	  (else (build (first pora)
		       (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
	  (else (+ (length* (first pora))
		   (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
	  (else (+ (* (weight* (first pora)) 2)
		   (weight* (second pora)))))))

(print (weight* '((a b) c)))
(print (weight* '(a (b c))))


(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
	  ((pair? (first pora))
	     (shuffle (revpair pora)))
	  (else (build (first pora)
		       (shuffle (second pora)))))))

(print (shuffle '(a (b c))))
(print (shuffle '(a b)))
;(print (shuffle '((a b) (c d))))

(define C
  (lambda (n)
    (cond ((one? n) 1)
	  ((even? n) (C (/ n 2)))
	  (else (C (+ 1 (* 3 n)))))))

(print (C 53))
(print (C 31))

(define A
  (lambda (n m)
    (cond ((zero? n) (+ 1 m))
	  ((zero? m) (A (- 1 n) 1))
	  (else (A (- 1 n)
		   (A n (- 1 m)))))))

(print (A 1 0))
;(print (A 2 2)) ;Aborting!: maximum recursion depth exceeded


;; Determines the length of the empty list
(print ((lambda (l)
	  (cond ((null? l) 0)
		(else (+ 1 (eternity (cdr l))))))
	'()))
;; Determines if a list if of length is <= 1
(print ((lambda (l)
	  (cond ((null? l) 0)
		(else (+ 1 ((lambda (l)
			      (cond ((null? l) 0)
				    (else (+ 1 (eternity (cdr l))))))
			    (cdr l))))))
	'(a)))


;; Determines if length l <= 2
(print ((lambda (l)
	  (cond ((null? l) 0)
		(else (+ 1 ((lambda (l)
			      (cond ((null? l) 0)
				    (else (+ 1 ((lambda (l)
						  (cond ((null? l) 0)
							(else (+ 1 (eternity (cdr l))))))
						(cdr l))))))
		    (cdr l))))))
	'(a b)))


;; length0 again
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l)))))))
 eternity)

;; length1 in the same style
((lambda (f)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond ((null? l) 0)
	    (else (+ 1 (g (cdr l)))))))
  eternity))

;; length2 in the same style
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
	    (else (+ 1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
	     (else (+ 1 (length (cdr l)))))))
   eternity)))

;; start to remove the repetition: length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l))))))))

;; length1
((lambda (mk-length)
   (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l))))))))

;; length2
((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l))))))))

;; length3
((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length
	 (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l))))))))

;; since the recursion breaks on application of `eternity`
;; and we don't care what function is applied to `mk-length`
;; just use `mk-length`
;; length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (mk-length (cdr l))))))))

;; length1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 ((mk-length eternity)
		       (cdr l))))))))

(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 ((mk-length eternity)
		       (cdr l))))))))
 '(apples))

;; Apply `mk-length` to itself in the inner lambda instead of `eternity`
;; This is length now.
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 ((mk-length mk-length)
		       (cdr l))))))))

;; although it doesn't look like length. the self application is
;; mixed up with the details of the length function. Instead, pull
;; this self application out.

;((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   ((lambda (length)
;      (lambda (l)
;	(cond ((null? l) 0)
;	      (else (+ 1 (length (cdr l)))))))
;   (mk-length mk-length))))

;; Because we are straight away applying `mk-length` to itself, the
;; recursion doesn't halt. Need to abstract out a function instead.

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1
		    ((lambda (x)
		       ((mk-length mk-length) x))
		     (cdr l))))))))

;; Move this new lambda out one level to get back `length`
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond ((null? l) 0)
	      (else (+ 1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; Extract this out some more
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (+ 1 (length (cdr l))))))))

;; Separate the function that makes `length` from the function
;; that looks like `length`
;;
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
	   ((mk-length mk-length) x))))))

;; The Y combinator (applicative order)
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

((y (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
	     (else (+ 1 (length (cdr l))))))))
'(a b c d e f g h i j k l m n o p q r s t u v w x y z))
