(define print
  (lambda (printable)
    (display printable)
    (newline)))

(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))))
