(define first car)
(define rest-of cdr)

(define make-exact-number
  (lambda (str)
    (string->number str)))

(define variable?
  (lambda (exp)
    (and (pair? exp)
	 (eq? (car exp)
	      'var-exp))))

(define get-var-name
  (lambda (var-exp)
    (symbol->string (cadr var-exp))))



;; change - to _ in syms
;; change -> to _to_ in syms
