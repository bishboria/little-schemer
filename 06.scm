(load "helpers")

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
	  (else (and (numbered? (car aexp))
		     (numbered? (car (cdr (cdr aexp)))))))))

(print (numbered? '(1 + 3)))
(print (numbered? '(3 + (4 expt 5))))
(print (numbered? '(2 x sausage)))
(print (numbered? '(3 + (4 x 5))))
(print (numbered? 1))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (cadr nexp) (quote +))
	     (+ (value (car nexp))
		(value (caddr nexp))))
	  ((eq? (cadr nexp) (quote x))
	     (* (value (car nexp))
		(value (caddr nexp))))
	  (else (expt (value (car nexp))
			 (value (caddr nexp)))))))

(print (value 13))
(print (value '(1 + 3)))
(print (value '(1 + (3 expt 4))))


;; The Seventh Commandment
;;
;; Recur on the subparts that are of the same nature:
;;   On the sublists of a list
;;   On the subexpressions of an arithmetic expression


(define value-v2
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (car nexp) '+)
	     (+ (value-v2 (cadr nexp))
		(value-v2 (caddr nexp))))
	  ((eq? (car nexp) '*)
	     (* (value-v2 (cadr nexp))
		(value-v2 (caddr nexp))))
	  (else (expt (value-v2 (cadr nexp))
		      (value-v2 (caddr nexp)))))))

(print (value-v2 '(+ 1 3)))
(print (value-v2 '(+ 1 (* 3 4))))
(print (value-v2 '(+ 1 (expt 3 4))))


(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-v3
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (operator nexp) '+)
	     (+ (value-v3 (1st-sub-exp nexp))
		(value-v3 (2nd-sub-exp nexp))))
	  ((eq? (operator nexp) '*)
	     (* (value-v3 (1st-sub-exp nexp))
		(value-v3 (2nd-sub-exp nexp))))
	  (else (expt (value-v3 (1st-sub-exp nexp))
		      (value-v3 (2nd-sub-exp nexp)))))))


;; The Eigth Commandment
;;
;; Use help functions to abstarct from reprsentations

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons () n)))

(define zub1
  (lambda (n)
    (cdr n)))

(print (sero? '()))
(print (zub1 '(())))


(define my-+
  (lambda (n m)
    (cond ((sero? m) n)
	  (else (edd1 (my-+ n (zub1 m)))))))
