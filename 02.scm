(load "helpers") ; Want `atom?` instead of having to retype it.

(define (lat? l)
  (cond ((null? l) #t)
        ((atom? (car l)) (lat? (cdr l)))
        (else #f)))

(display (eq? (lat? '(Jack Sprat could eat no chicken fat)) #t))
(display (eq? (lat? '((Jack) Sprat could eat no chicken fat)) #f))
(display (eq? (lat? '(Jack (Sprat could) eat no chicken fat)) #f))
(display (eq? (lat? ()) #t))

(display (or (null? ()) (atom? '(d e f g))))
(display (or (null? '(a b c)) (null? ())))
(display (or (null? '(a b c)) (null? '(atom))))  ; #f

(display (member? 'tea '(coffee tea or milk)))
(display (member? 'poached '(fried eggs and scrambled eggs))) ; #f
(display (member? 'meat '(mashed potatoes and meat gravy)))


;; The First Commandment
;;
;; Always ask `null?` as the first question in expressing any function

