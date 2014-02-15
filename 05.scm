(load "helpers")

(define rember*
  (lambda (a l)
    (cond ((null? l) l)
	  ((atom? (car l))
	     (cond ((eq? (car l) a) (rember* a (cdr l)))
		   (else (cons (car l)
			       (rember* a (cdr l))))))
	  (else (cons (rember* a (car l))
		      (rember* a (cdr l)))))))

(print (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)))
(print (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))

(define insertR*
  (lambda (new old l)
    (cond ((null? l) l)
	  ((atom? (car l))
	     (cond ((eq? (car l) old) (cons old
					    (cons new
						  (insertR* new old (cdr l)))))
		   (else (cons (car l)
			       (insertR* new old (cdr l))))))
	  (else (cons (insertR* new old (car l))
		      (insertR* new old (cdr l)))))))

(print (insertR* 'roast
		 'chuck
		 '((how much (wood))
		   could
		   ((a (wood) chuck))
		   (((chuck)))
		   (if (a) ((wood chuck)))
		   could chuck wood)))


;; The First Commandment
;;
;; (final version)
;;
;; When recurring on a list of atoms, `lat`, ask two questions
;; about it: `(null? lat)` and `else`.
;; When recurring on a number, `n`, ask two questions about it:
;; `(zero? n)` and `else`.
;; When recurring on a list of S-expressions, `l`, ask three
;; questions about it: `(null? l)`, `(atom? (car l))`, and `else`.


;; The Fourth Commandment
;;
;; (final version)
;;
;; Always change at least one argument fhile recurring. When recurring on a list
;; of atoms, `lat`, use `(cdr lat)`. When recurring on a number, `n`, use
;; `(sub1 n)`. And when recurring on a list of S-expressions, `l`, use `(car l)`
;; and `(cdr l)` if neither `(null? l)` nor `(atom? (car l))` are true.
;;
;; It must be changed to be closer to termination. The changing argument must be
;; tested in the termiation condition:
;;
;; when using `cdr`, test termiation with `null?` and
;; when using `sub1`, test termiation with `zero?`.

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
	  ((atom? (car l))
	     (cond ((eq? (car l) a) (1+ (occur* a (cdr l))))
		   (else (occur* a (cdr l)))))
	  (else (+ (occur* a (car l))
		   (occur* a (cdr l)))))))

(print (occur* 'banana '((banana)
			 (split ((((banana ice)))
				 (cream (banana))
				 sherbet))
			 (banana)
			 (bread)
			 (banana brandy))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) l)
	  ((atom? (car l))
	     (cond ((eq? (car l) old) (cons new
					    (subst* new old (cdr l))))
		   (else (cons (car l)
			       (subst* new old (cdr l))))))
	  (else (cons (subst* new old (car l))
		      (subst* new old (cdr l)))))))

(print (subst* 'orange 'banana '((banana)
				 (split ((((banana ice)))
					 (cream (banana))
					 sherbet))
				 (banana)
				 (bread)
				 (banana brandy))))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) l)
	  ((atom? (car l))
	     (cond ((eq? (car l) old) (cons new
					    (cons old
						  (insertL* new old (cdr l)))))
		   (else (cons (car l)
			       (insertL* new old (cdr l))))))
	  (else (cons (insertL* new old (car l))
		      (insertL* new old (cdr l)))))))

(print (insertL* 'pecker 'chuck '((how much (wood))
				  could
				  ((a (wood) chuck))
				  (((chuck)))
				  (if (a) ((wood chuck)))
				  could chuck wood)))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
	  ((atom? (car l))
	     (or (eq? (car l) a)
		 (member* a (cdr l))))
	  (else (or (member* a (car l))
		    (member* a (cdr l)))))))

(print (member* 'chips '((potato) (chips ((with) fish) (chips)))))

(define leftmost
  (lambda (l)
    (cond ((null? l) l)
	  ((atom? (car l)) (car l))
	  (else (leftmost (car l))))))

(print (leftmost '((potato) (chips ((with) fish) (chips)))))
(print (leftmost '(((hot) (tuna (and))) cheese)))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  (else (and (my-equal? (car l1) (car l2))
		     (eqlist? (cdr l1) (cdr l2)))))))

(define my-equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
	     (eq? s1 s2))
	  (or (atom? s1) (atom? s2)) #f)
	  (else (eqlist? s1 s2)))))

(print (eqlist? '(strawberry ice cream)
		'(strawberry ice cream)))
(print (eqlist? '(strawberry ice cream)
		'(strawberry cream ice)))
(print (eqlist? '(banana ((split)))
		'((banana) (split))))
(print (eqlist? '(beef ((sausage)) (and (soda)))
		'(beef ((salami)) (and (soda)))))
(print (eqlist? '(beef ((sausage)) (and (soda)))
		'(beef ((sausage)) (and (soda)))))


;; The Sixth Commandment
;;
;; Simplify only after the function is correct

(define rember-v2
  (lambda (s l)
    (cond ((null? l) l)
	  ((my-equal? (car l s)) (cdr l))
	  (else (cons (car l)
		      (rember-v2 s (cdr l)))))))
