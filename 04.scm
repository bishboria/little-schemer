(load "01")

(atom? 14)
(atom? -3)

(define add1
  (lambda (n)
    (+ n 1)))

(print (add1 67))

(define sub1
  (lambda (n)
    (- n 1)))

(print (sub1 0)) ; book says no answer...

(print (zero? 0))
(print (zero? 1492))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
	  (else (add1 (o+ n (sub1 m)))))))

(print (o+ 46 12))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
	  (else (sub1 (o- n (sub1 m)))))))

(print (o- 14 3))
(print (o- 17 9))
(print (o- 18 25)) ; book says no answer.
		   ; It wants us to treat whole numbers as Naturals

'(2 11 3 79 47 6)


;; The First Commandment
;;
;; (first revision)
;;
;; When recurring on a list of atoms, lat, ask two questions
;; about it: (null? lat) and else.
;; When recurring on a number, n, ask two questions about it:
;; (zero? n) and else.

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
	  (else (o+ (car tup) (addtup (cdr tup)))))))

(print (addtup '(3 5 2 8)))
(print (addtup '(15 6 7 12 3)))

;; The Fourth Commandment
;;
;; (first revision)
;;
;; Always change at least one argument while recurring. It
;; must be changed to be closer to termination. The changing
;; argument must be tested in the termination condition:
;; when using `cdr`, test termination with `null?` and
;; when using `sub1`, test termination with `zero?`

(define x
  (lambda (n m)
    (cond ((zero? m) 0)
	  (else (+ n (x n (sub1 m)))))))

(print (x 5 3))
(print (x 13 4))
(print (x 12 3))


;; The Fifth Commandment
;;
;; When building a value `o+` always use 0 for the value of the
;; therminating line, for adding 0 does not change the value of
;; an addition.
;;
;; When building a value with `x` always use 1 for the value of
;; the terminating line, for multiplying by 1 does not change
;; the value of a multiplication.
;;
;; When building a value with `cons`, always consider `()` for
;; the value of the terminating line.

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
	  ((null? tup2) tup1)
	  (else (cons (o+ (car tup1)
			  (car tup2))
		      (tup+ (cdr tup1)
			    (cdr tup2)))))))

(print (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))
(print (tup+ '(2 3) '(4 6)))
(print (tup+ '(3 7) '(4 6)))
(print (tup+ '(3 7 8 1) '(4 6)))

(define o>
  (lambda (n m)
    (cond ((zero? n) #f)
	  ((zero? m) #t)
	  (else (o> (sub1 n) (sub1 m))))))


(print (o> 12 133))
(print (o> 120 11))
(print (o> 6 6))

(define o<
  (lambda (n m)
    (cond ((zero? m) #f)
	  ((zero? n) #t)
	  (else (o< (sub1 n) (sub1 m))))))

(print (o< 12 133))
(print (o< 120 11))
(print (o< 6 6))

(define =v1
  (lambda (n m)
    (cond ((zero? m) (zero? n))
	  ((zero? n) #f)
	  (else (=v1 (sub1 n) (sub1 m))))))

(define =v2
  (lambda (n m)
    (cond ((o> n m) #f)
	  ((o< n m) #f)
	  (else #t))))

(define my-expt
  (lambda (b e)
    (cond ((zero? e) 1)
	  (else (x b (my-expt b (sub1 e)))))))

(print (my-expt 1 1))
(print (my-expt 2 3))
(print (my-expt 5 3))


(define ???
  (lambda (n m)
    (cond ((< n m) 0)
	  (else (add1 (??? (o- n m) m)))))) ;; Looks live division...

          ; (??? 6 3)
	  ; (add1 (??? 3 3))
	  ; (add1 (add1 (??? 0 3)))
	  ; (add1 (add1 0))
	  ; 2

(define div ???)

(print (div 15 4))

(define my-length
  (lambda (lat)
    (cond ((null? lat) 0)
	  (else (add1 (my-length (cdr lat)))))))

(print (length '(hotdogs with mustard sauerkraut and pickles)))
(print (length '(ham and cheese on rye)))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
	  (else (pick (sub1 n) (cdr lat))))))

(print (pick 4 '(lasagna spaghetti ravioli macaroni meatball)))
;(print (pick 0 ())) ; Can't take `cdr` of `()`

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
	   (else (cons (car lat)
		       (rempick (sub1 n)
				(cdr lat)))))))

(print (rempick 3 '(hotdogs with hot mustard)))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) lat)
	  ((number? (car lat)) (no-nums (cdr lat)))
	  (else (cons (car lat)
		      (no-nums (cdr lat)))))))

(print (no-nums '(5 pears 6 prunes 9 dates)))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) lat)
	  ((number? (car lat)) (cons (car lat)
				     (all-nums (cdr lat))))
	  (else (all-nums (cdr lat))))))

(print (all-nums '(5 pears 6 prunes 9 dates)))


(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (=v2 a1 a2))
	  (else (eq? a1 a2)))))

(print (eqan? 3 4))
(print (eqan? 3 3))
(print (eqan? 3 'a))
(print (eqan? 'a 'a))


(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
	  ((eq? (car lat) a) (add1 (occur a (cdr lat))))
	  (else (occur a (cdr lat))))))

(print (occur 'a '(b c d a a f b)))

(print (one? 1))
(print (one? 2))

(define rempick-v2
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
	  (else (cons (car lat)
		      (rempick-v2 (sub1 n)
				  (cdr lat)))))))

(print (rempick-v2 3 '(lemon meringue salty pie)))
