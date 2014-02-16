(load "helpers")

(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
	  ((member? (car lat) (cdr lat)) #f)
	  (else (set? (cdr lat))))))

(print (set? '(apple peaches apple plum)))
(print (set? '(apples peaches pears plums)))
(print (set? ()))
(print (set? '(apple 3 pear 4 9 apple 3 4)))

(define makeset
  (lambda (lat)
    (cond ((null? lat) lat)
	  ((member? (car lat) (cdr lat))
	     (makeset (cdr lat)))
	  (else (cons (car lat)
		      (makeset (cdr lat)))))))

(print (makeset '(apple peach pear peach plum apple lemon peach)))

(define makeset-v2
  (lambda (lat)
    (cond ((null? lat) lat)
	  (else (cons (car lat)
		      (makeset-v2 (multirember (car lat)
					       (cdr lat))))))))

(print (makeset-v2 '(apple peach pear peach plum apple lemon peach)))
(print (makeset-v2 '(apple 3 pear 4 9 apple 3 4)))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
	  ((member? (car set1) set2)
	     (subset? (cdr set1) set2))
	  (else #f))))

(print (subset? '(5 chicken wings)
		'(5 hamburgers 2 pieces fried chicken and light duckling wings)))
(print (subset? '(4 pounds of horseradish)
		'(four pounds chicken and 5 ounces horseradish)))

(define eqset?
  (lambda (set1 set2)
    (cond ((subset? set1 set2)
	     (subset? set2 set1))
	  (else #f))))

(print (eqset? '(6 large chickens with wings)
	       '(6 chickens with large wings)))

(define eqset-v2?
  (lambda (s1 s2)
    (cond (else (and (subset? s1 s2)
		     (subset? s2 s1))))))

(print (eqset-v2? '(6 large chickens with wings)
		  '(6 chickens with large wings)))

(define eqset-v3?
  (lambda (s1 s2)
    (and (subset? s1 s2)
	 (subset? s2 s1))))

(print (eqset-v3? '(6 large chickens with wings)
		  '(6 chickens with large wings)))

(define intersect?
  (lambda (s1 s2)
    (cond ((null? s1) #f)
	  (else (or (member? (car s1) s2)
		    (intersect? (cdr s1) s2))))))

(print (intersect? '(stewed tomatoes and macaroni)
		   '(macaroni and cheese)))
(print (intersect? '(stewed)
		   '(beef burger)))

(define intersect
  (lambda (s1 s2)
    (cond ((null? s1) s1)
	  ((member? (car s1) s2)
	     (cons (car s1)
		   (intersect (cdr s1) s2)))
	  (else (intersect (cdr s1) s2)))))

(print (intersect '(stewed tomatoes and macaroni)
		  '(macaroni and cheese)))

(define union
  (lambda (s1 s2)
    (cond ((null? s1) s2)
	  ((member? (car s1) s2)
	     (union (cdr s1) s2))
	  (else (cons (car s1)
		      (union (cdr s1) s2))))))

(print (union '(stewed tomatoes and macaroni casserole)
	      '(macaroni and cheese)))

(define difference
  (lambda (s1 s2)
    (cond ((null? s1) s1)
	  ((member? (car s1) s2)
	     (difference (cdr s1) s2))
	  (else (cons (car s1)
		      (difference (cdr s1) s2))))))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
	  (else (intersect (car l-set)
			   (intersectall (cdr l-set)))))))

(print (intersectall '((a b c) (c a d e) (e f g h a b))))
(print (intersectall '((6 pears and)
		       (3 peaches and 6 peppers)
		       (8 pears and 6 plums)
		       (and 6 prunes with some apples))))

(define a-pair?
  (lambda (x)
    (cond ((null? x) #f)
	  ((null? (cdr x)) #f)
	  ((null? (cddr x)) #t)
	  ((atom? x) #f)
	  (else #f))))

(print (a-pair? '(3 7)))
(print (a-pair? '((2) (pair))))
(print (a-pair? '(full (house))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 ()))))

(define third
  (lambda (p)
    (caddr p)))

;; (print (rel? '(apples peaches pumpkin pie)))
;; (print (rel? '((apples peaches)
;; 	       (pumpkin pie)
;; 	       (apples peaches))))
;; (print (rel? '((apples peaches)
;; 	       (pumpkin pie))))
;; (print (rel? '((4 3) (4 2) (7 6) (6 2) (3 4))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(print (fun? '((4 3) (4 2) (7 6) (6 2) (3 4))))
(print (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(print (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) rel)
	  (else (cons (build (second (car rel))
			     (first (car rel)))
		      (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p)
	   (first p))))

(define revrel-v2
  (lambda (rel)
    (cond ((null? rel) rel)
	  (else (cons (revpair (car rel))
		      (revrel-v2 (cdr rel)))))))

(print (revrel '((8 a) (pumpkin pie) (got sick))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define fullfun-v2?
  (lambda (fun)
    (fun? (revrel-v2 fun))))

(print (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(print (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
(print (fullfun? '((grape raisin)
		   (plum prune)
		   (stewed prune))))
(print (fullfun? '((grape raisin)
		   (plum prune)
		   (stewed grape))))
(print (fullfun-v2? '((chocolate chip)
		      (cookie dough))))
