(define print
  (lambda (printable)
    (display printable)
    (newline)))

(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) lat)
	  ((equal? (car lat) a) (multirember a (cdr lat)))
	  (else (cons (car lat)
		      (multirember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond ((null? l) l)
	  (else (cons (car (car l))
		      (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond ((null? l) l)
	  (else (cons (car (cdr (car l)))
		      (seconds (cdr l)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (car aexp)))

'helpers-loaded
