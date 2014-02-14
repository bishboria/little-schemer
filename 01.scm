(load "helpers")

(quote atom)
'turkey
1492
'u
'*abc$
'(atom)
'(atom turkey or)
'((atom turkey) or)
'(((how) are) ((you) (doing so)) far) ; 3 S-expressions
'()
'(() () () ())
; (car 'hotdogs) error
(car '(a b c))
(car '((a b c) x y z))

;; Law of Car
;;
;; The primitive *car* is defined only for non-empty lists
(define l '(((hotdogs)) (and) (pickle) relish))
(car l)
(car (car l))
(cdr '(a b c))
(cdr '((a b c) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
; (cdr 'hotdogs) error
; (cdr '()) error

;; Law of Cdr
;;
;; The primitive *cdr* is defined only for non-empty lists.
;; The cdr of any non-empty list is always another list.

(car (cdr '((b) (x y) ((c)))))
(cdr (cdr '((b) (x y) ((c)))))
; (cdr (car '(a (b (c)) d))) error
(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())
(cons '((a b c)) 'b) ; doesn't really crash. Dotted pair
(cons 'a 'b) ; dotted pair (a . b)

;; Law of cons
;;
;; The primitive *cons* takes two arguments.
;; The second argument to cons "must" be a list. The result is a list

(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))
(null? '())
(null? (quote ()))
(null? '(a b c))
(null? 'spaghetti) ; returns #t

;; Law of null?
;;
;; The primitive *null?* is defined "only" for lists.


(atom? 'Harry)
(atom? '(Harry had a heap of apples))
; (atom? 'Harry 'Sally) error too many arguments
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing (low sweet) cherry oat))))
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry)) ; returns #f
(eq? 6 7) ; returns #f

;; Law of eq?
;;
;; The primitive *eq?* takes two arguments.
;; Each "must" be a non-numeric "atom"

(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)
(set! l '(beans beans we need jelly beans))
(eq? (car l) (car (cdr l)))
