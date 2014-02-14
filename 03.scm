(define rember
  (lambda (a l)
    (cond ((null? l) ())
          ((eq? a (car l)) (cdr l))
          (else (cons (car l)
                      (rember a (cdr l)))))))

(print (rember 'mint '(lamb chops and mint jelly)))
(print (rember 'mint '(lamb chops and mint flavored mint jelly)))
(print (rember 'toast '(bacon lettuce and tomato)))
(print (rember 'sauce '(soy sauce and tomato sauce)))


;; The Second Commandment
;;
;; Use cons to build lists

(define firsts
  (lambda (l)
    (cond ((null? l) l)
	  (else (cons (car (car l))
		      (firsts (cdr l)))))))

(print (firsts '((apple peach pumpkin)
		 (plum pear cherry)
		 (grape raisin pea)
		 (bean carrot egglant))))
(print (firsts '((a b)
		 (c d)
		 (e f))))
(print (firsts ()))
(print (firsts '((five plums)
		 (four)
		 (eleven green oranges))))
(print (firsts '(((five plums) four)
		 (eleven green oranges)
		 ((no) more))))

;; The Third Commandment
;;
;; When building a list, describe the first typical element,
;; and then `cons` it onto the natural recursion.

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) old)
	     (cons old
		   (cons new (cdr lat))))
	  (else
	     (cons (car lat)
		   (insertR new old (cdr lat)))))))

(print (insertR 'topping 'fudge '(ice cream with fudge for dessert)))
(print (insertR 'jalapeno 'and '(tacos tamales and salsa)))
(print (insertR 'e 'd '(a b c d f g h)))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) old)
	     (cons new lat))
	  (else (cons (car lat)
		      (insertL new old (cdr lat)))))))

(print (insertL 'c 'd '(a b d)))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) old)
	     (cons new (cdr lat)))
	  (else (cons (car lat)
		      (subst new old (cdr lat)))))))

(print (subst 'c 'd '(a b d)))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) lat)
	  ((or (eq? (car lat) o1)
	       (eq? (car lat) o2))
	     (cons new (cdr lat)))
	  (else (cons (car lat)
		      (subst2 new o1 o2 (cdr lat)))))))

(print (subst2 'vanilla 'chococlat 'banana '(banana ice cream with chocolate topping)))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) a) (multirember a (cdr lat)))
	  (else (cons (car lat)
		      (multirember a (cdr lat)))))))

(print (multirember 'cup '(coffee cup tea cup and hick cup)))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) old)
	     (cons old
		   (cons new
			 (multiinsertR new old (cdr lat)))))
	  (else (cons (car lat)
		      (multiinsertR new old (cdr lat)))))))

(print (multiinsertR 'foo 'bar '(bar baz bar bar)))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) old)
	     (cons new
		   (cons old
			 (multiinsertL new old (cdr lat)))))
	  (else (cons (car lat)
		      (multiinsertL new old (cdr lat)))))))

(print (multiinsertL 'foo 'bar '(bar baz bar bar)))

;; The Fourth Commandment
;; (preliminary)
;; Always change at least one argument while recurring. It
;; must be changed to be closer to termination. The changing
;; argument must be tested in the termination condition:
;; when using `cdr`, test termination with `null?`

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) lat)
	  ((eq? (car lat) old)
	     (cons new
		   (multisubst new old (cdr lat))))
	  (else (cons (car lat) (multisubst new old (cdr lat)))))))

(print (multisubst '1 '2 '(9 2 2 9 2 2 9)))
