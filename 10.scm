(load "helpers")

(define new-entry build)

(print (new-entry '(appetizer entree beverage)
		  '(pate boeuf vin)))
(print (new-entry '(appetizer entree beverage)
		  '(beer beer beer)))
(print (new-entry '(beverage dessert)
		  '((food is) (number one with us))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
	  ((eq? (car names) name) (car values))
	  (else (lookup-in-entry-help name
				      (cdr names)
				      (cdr values)
				      entry-f)))))

(print (lookup-in-entry 'entree '((appetizer entree beverage)
				  (food tastes good))
			(lambda (x)
			  x)))

;; a table (aka environment) is a list of entries

;; takes an entry and a table
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
	  (else (lookup-in-entry name
				 (car table)
				 (lambda (name)
				   (lookup-in-table name
						    (cdr table)
						    table-f)))))))

(print (lookup-in-table 'entree
			'(((entree dessert)
			   (spaghetti spumoni))
			  ((appetizer entree beverage)
			   (food tastes good)))
			(lambda (name) name)))

(print (cons 'a
	     (cons 'b
		   (cons 'c
			 '()))))



(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
	  (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
	  ((eq? e #t) *const)
	  ((eq? e #f) *const)
	  ((eq? e (quote cons)) *const)
	  ((eq? e (quote car)) *const)
	  ((eq? e (quote cdr)) *const)
	  ((eq? e (quote null?)) *const)
	  ((eq? e (quote eq?)) *const)
	  ((eq? e (quote atom?)) *const)
	  ((eq? e (quote zero?)) *const)
	  ((eq? e (quote add1)) *const)
	  ((eq? e (quote sub1)) *const)
	  ((eq? e (quote number?)) *const)
	  (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e)) (cond ((eq? (car e) (quote quote)) *quote)
				 ((eq? (car e) (quote lambda)) *lambda)
				 ((eq? (car e) (quote cond)) *cond)
				 (else *application)))
	  (else *application))))

(print (list-to-action '(quote foo)))
(print (list-to-action '((lambda (x) (cons 1 x)) 2)))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond ((number? e) e)
	  ((eq? e #t) #t)
	  ((eq? e #f) #f)
	  (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (error '(got to the end of the table looking for)
	  name)))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
	   (cons table (cdr e)))))

(print (meaning '(lambda (x) (cons x y))
		'(((y z) ((8) 9)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines)))
	     (meaning (answer-of (car lines))
		      table))
	  ((meaning (question-of (car lines))
		    table)
	     (meaning (answer-of (car lines))
		      table))
	  (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else)))
	  (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

(print (*cond '(cond (coffee klatsch)
		     (else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6))))))

(define evlis
  (lambda (args table)
    (cond ((null? args) '())
	  (else (cons (meaning (car args) table)
		      (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
	   (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun)
					     vals))
	  ((non-primitive? fun) (apply-closure (second fun)
					       vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name 'cons)
	     (cons (first vals) (second vals)))
	  ((eq? name 'car)
	     (car (first vals)))
	  ((eq? name 'cdr)
	     (cdr (first vals)))
	  ((eq? name 'null?)
	     (null? (first vals)))
	  ((eq? name 'eq?)
	     (eq? (first vals) (second vals)))
	  ((eq? name 'atom?)
	     (:atom? (first vals)))
	  ((eq? name 'zero?)
	     (zero? (first vals)))
	  ((eq? name 'add1)
	     (+ 1 (first vals)))
	  ((eq? name 'sub1)
	     (- 1 (first vals)))
	  ((eq? name 'number?)
	     (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond ((atom? x) #t)
	  ((null? x) #f)
	  ((eq? (car x) 'primitive)
	     #t)
	  ((eq? (car x) 'non-primitive)
	     #t)
	  (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table (new-entry (formals-of closure)
				      vals)
			   (table-of closure)))))

(print (apply-closure '((((u v w)
			  (1 2 3))
			 ((x y z)
			  (4 5 6)))
			(x y)
			(cons z x))
		      '((a b c) (d e f))))
(print (meaning 'cons '()))

(define atom-list
  (cons 'car
	(cons (cons 'quote
		    (cons (cons 'a
				(cons 'b
				      (cons 'c
					    '())))
			  '()))
	      '())))

(print atom-list)
(print (value '(car '(a b c))))
(print (value '((lambda (x) (cons 1 x)) 2)))
(print (value '((lambda (nothing)
		 (cons nothing '()))
		'(from nothing comes something))))
(print (value '((lambda (nothing)
		 (cond (nothing (quote something))
		       (else (quote nothing))))
	       #t)))
(print (value '((lambda (nothing)
		  (cond (nothing (quote something))
			(else (quote nothing))))
		#f)))
(print (value #f))
(print (value #t))
