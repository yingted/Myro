;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader-cps.ss")
(load "unifier-cps.ss")

;;--------------------------------------------------------------------------
;; The core grammar
;;
;; <exp> ::= <literal>
;;         | (quote <datum>)
;;         | (quasiquote <datum>)
;;         | <var>
;;         | (if <exp> <exp> <exp>)
;;         | (set! <var> <exp>)
;;         | (define <var> <exp>)
;;         | (define-syntax <keyword> (<pattern> <pattern>) ...)
;;         | (begin <exp> ...)
;;         | (lambda (<formal> ...) <exp> ...)
;;         | (lambda <formal> <exp> ...)
;;         | (<exp> <exp> ...)

;;         | (try <body> (catch <var> <exp> ...))
;;         | (try <body> (finally <exp> ...))
;;         | (try <body> (catch <var> <exp> ...) (finally <exp> ...))

;;         | (raise <exp>)

;;--------------------------------------------------------------------------

(define-datatype expression expression?
  (lit-exp
   (datum anything?))
  (var-exp
    (id symbol?))
  (if-exp
   (test-exp expression?)
   (then-exp expression?)
   (else-exp expression?))
  (assign-exp
    (var symbol?)
    (rhs-exp expression?))
  (define-exp
    (id symbol?)
    (rhs-exp expression?))
  (define-syntax-exp
    (keyword symbol?)
    (clauses (list-of (list-of pattern?))))
  (begin-exp
    (exps (list-of expression?)))
  (lambda-exp
    (formals (list-of symbol?))
    (body expression?))
  (mu-lambda-exp
    (formals (list-of symbol?))
    (runt symbol?)
    (body expression?))
  (app-exp
    (operator expression?)
    (operands (list-of expression?)))
  (try-catch-exp
    (body expression?)
    (catch-var symbol?)
    (catch-exps (list-of expression?)))
  (try-finally-exp
    (body expression?)
    (finally-exps (list-of expression?)))
  (try-catch-finally-exp
    (body expression?)
    (catch-var symbol?)
    (catch-exps (list-of expression?))
    (finally-exps (list-of expression?)))
  (raise-exp
    (exp expression?))
  )

;;--------------------------------------------------------------------------
;; Macro support

(load "environments-cps.ss")

(define syntactic-sugar?
  (lambda (datum)
    (and (list? datum)
	 (not (null? datum))
	 (symbol? (car datum))
	 (search-env macro-env (car datum)))))

(define expand-once
  (lambda (datum handler k)
    (lookup-value (car datum) macro-env handler
      (lambda (result)
	(if (list? result)
	  (process-macro-clauses result datum handler k)
	  (k (result datum)))))))

(define process-macro-clauses
  (lambda (clauses datum handler k)
    (if (null? clauses)
      (handler (format "no matching clause found for ~a" datum))
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses)))
	(unify-patterns left-pattern datum
	  (lambda (subst)
	    (if subst
	      (instantiate right-pattern subst k)
	      (process-macro-clauses (cdr clauses) datum handler k))))))))

(define mit-define-transformer
  (lambda (datum)
    (let ((name (caadr datum))
	  (formals (cdadr datum))
	  (bodies (cddr datum)))
      `(define ,name (lambda ,formals ,@bodies)))))

(define let-transformer
  (lambda (datum)
    (if (symbol? (cadr datum))
      ;; named let
      (let* ((name (cadr datum))
	     (bindings (caddr datum))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cdddr datum)))
	`(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps)))
      ;; ordinary let
      (let* ((bindings (cadr datum))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cddr datum)))
	`((lambda ,vars ,@bodies) ,@exps)))))

(define letrec-transformer
  (lambda (datum)
    (let* ((decls (cadr datum))
	   (vars (map car decls))
	   (procs (map cadr decls))
	   (bodies (cddr datum))
	   (bindings (map (lambda (var) `(,var #f)) vars))
	   (assigns (map (lambda (var proc) `(set! ,var ,proc)) vars procs)))
      `(let ,bindings ,@assigns ,@bodies))))

(define make-macro-env
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let* 'let 'letrec)
      (list '([(and) #t]
	      [(and ?exp) ?exp]
	      [(and ?exp . ?exp*) (if ?exp (and . ?exp*) #f)])
	    '([(or) #f]
	      [(or ?exp) ?exp]
	      ;; incorrect:
	      ;;[(or ?exp . ?exp*) (if ?exp #t (or . ?exp*))]
	      [(or ?exp . ?exp*)
	       (let ((first ?exp) (rest (lambda () (or . ?exp*))))
		 (if first first (rest)))])
	    ;; not quite correct:
	    '([(cond) #t]
	      [(cond (else . ?results)) (begin . ?results)]
	      [(cond (?test . ?results)) (if ?test (begin . ?results) #f)]
	      [(cond (?test . ?results) . ?rest)
	       (if ?test (begin . ?results) (cond . ?rest))])
	    '([(let* () . ?bodies) (begin . ?bodies)]
	      [(let* ((?var ?exp) . ?rest) . ?bodies)
	       (let ((?var ?exp)) (let* ?rest . ?bodies))])
	    let-transformer
	    letrec-transformer))))

(define macro-env (make-macro-env))

;;--------------------------------------------------------------------------

;; for testing purposes
(define test-handler
  (lambda (e) (list 'exception e)))

;; for testing purposes
(define parse-string
  (lambda (string)
    (read-datum string test-handler
      (lambda (datum tokens-left)
	(parse datum test-handler (lambda (exp) exp))))))

(define parse
  (lambda (datum handler k)
    (cond
      ((literal? datum) (k (lit-exp datum)))
      ((quote? datum) (k (lit-exp (cadr datum))))
      ((quasiquote? datum)
       (expand-quasiquote (cadr datum) handler
	 (lambda (v)
	   (parse v handler k))))
      ((unquote? datum) (handler (format "misplaced ~a" datum)))
      ((unquote-splicing? datum) (handler (format "misplaced ~a" datum)))
      ((symbol? datum) (k (var-exp datum)))
      ((syntactic-sugar? datum)
       (expand-once datum handler
	 (lambda (v)
	   (parse v handler k))))
      ((if-then? datum)
       (parse (cadr datum) handler
	 (lambda (v1)
	   (parse (caddr datum) handler
	     (lambda (v2)
	       (k (if-exp v1 v2 (lit-exp #f))))))))
      ((if-else? datum)
       (parse (cadr datum) handler
	 (lambda (v1)
	   (parse (caddr datum) handler
	     (lambda (v2)
	       (parse (cadddr datum) handler
		 (lambda (v3)
		   (k (if-exp v1 v2 v3)))))))))
      ((assignment? datum)
       (parse (caddr datum) handler
	 (lambda (v)
	   (k (assign-exp (cadr datum) v)))))
      ((define? datum)
       (if (mit-define? datum)
	 (parse (mit-define-transformer datum) handler k)
	 (parse (caddr datum) handler
	   (lambda (body)
	     (k (define-exp (cadr datum) body))))))
      ((define-syntax? datum)
       (k (define-syntax-exp (cadr datum) (cddr datum))))
      ((begin? datum)
       (parse-all (cdr datum) handler
	 (lambda (v)
	   (k (begin-exp v)))))
      ((lambda? datum)
       (parse (cons 'begin (cddr datum)) handler
	 (lambda (body)
	   (if (proper-list? (cadr datum))
	     (k (lambda-exp (cadr datum) body))
	     (k (mu-lambda-exp (head (cadr datum)) (last (cadr datum)) body))))))
      ((try? datum)
       (cond
	 ((= (length datum) 2)
	  ;; (try <body>)
	  (parse (try-body datum) handler k))
	 ((and (= (length datum) 3) (catch? (caddr datum)))
	  ;; (try <body> (catch <var> <exp> ...))
	  (parse (try-body datum) handler
	    (lambda (body)
	      (parse-all (catch-exps (caddr datum)) handler
		(lambda (cexps)
		  (let ((cvar (catch-var (caddr datum))))
		    (k (try-catch-exp body cvar cexps))))))))
	 ((and (= (length datum) 3) (finally? (caddr datum)))
	  ;; (try <body> (finally <exp> ...))
	  (parse (try-body datum) handler
	    (lambda (body)
	      (parse-all (finally-exps (caddr datum)) handler
		(lambda (fexps)
		  (k (try-finally-exp body fexps)))))))
	 ((and (= (length datum) 4) (catch? (caddr datum)) (finally? (cadddr datum)))
	  ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	  (parse (try-body datum) handler
	    (lambda (body)
	      (parse-all (catch-exps (caddr datum)) handler
		(lambda (cexps)
		  (parse-all (finally-exps (cadddr datum)) handler
		    (lambda (fexps)
		      (let ((cvar (catch-var (caddr datum))))
			(k (try-catch-finally-exp body cvar cexps fexps))))))))))
	 (else (handler (format "bad try syntax: ~a" datum)))))
      ((raise? datum)
       (parse (cadr datum) handler
	 (lambda (v)
	   (k (raise-exp v)))))
      ((application? datum)
       (parse (car datum) handler
	 (lambda (v1)
	   (parse-all (cdr datum) handler
	     (lambda (v2)
	       (k (app-exp v1 v2)))))))
      (else (handler (format "bad concrete syntax: ~a" datum))))))

(define parse-all
  (lambda (datum-list handler k)
    (if (null? datum-list)
      (k '())
      (parse (car datum-list) handler
	(lambda (a)
	  (parse-all (cdr datum-list) handler
	    (lambda (b)
	      (k (cons a b)))))))))

(define expand-quasiquote
  (lambda (datum handler k)
    (cond
      ((vector? datum)
       (expand-quasiquote (vector->list datum) handler
	 (lambda (ls) (k `(list->vector ,ls)))))
      ((not (pair? datum)) (k `(quote ,datum)))
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) (k `(quote ,datum)))
      ((unquote? datum) (k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
	 (k (cadr (car datum)))
	 (expand-quasiquote (cdr datum) handler
	   (lambda (v) (k `(append ,(cadr (car datum)) ,v))))))
      (else
	(expand-quasiquote (car datum) handler
	  (lambda (v1)
	    (expand-quasiquote (cdr datum) handler
	      (lambda (v2)
		(k `(cons ,v1 ,v2))))))))))

(define proper-list?
  (lambda (x)
    (or (null? x)
	(and (pair? x)
	     (proper-list? (cdr x))))))

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((pair? (cdr formals)) (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define mit-define?
  (lambda (datum)
    (not (symbol? (cadr datum)))))

(define literal?
  (lambda (datum)
    (or (number? datum)
	(boolean? datum)
	(char? datum)
	(string? datum)
	(vector? datum))))

(define anything?
  (lambda (datum) #t))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (and (list? datum)
	   (op (length datum) len)
	   (eq? (car datum) tag)))))

(define quote? (tagged-list 'quote = 2))
(define quasiquote? (tagged-list 'quasiquote = 2))
(define unquote? (tagged-list 'unquote = 2))
(define unquote-splicing? (tagged-list 'unquote-splicing = 2))
(define if-then? (tagged-list 'if = 3))
(define if-else? (tagged-list 'if = 4))
(define assignment? (tagged-list 'set! = 3))
(define define? (tagged-list 'define >= 3))
(define define-syntax? (tagged-list 'define-syntax >= 3))
(define begin? (tagged-list 'begin >= 2))
(define lambda? (tagged-list 'lambda >= 3))
(define raise? (tagged-list 'raise = 2))

(define application?
  (lambda (datum)
    (and (list? datum)
	 (not (null? datum))
	 (not (reserved-keyword? (car datum))))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
	 (memq x '(quote quasiquote lambda if set! define begin
		    cond and or let let* letrec ;; do delay case
		    try catch finally raise
		    )))))

(define try? (tagged-list 'try >= 2))
(define try-body cadr)
(define catch? (tagged-list 'catch >= 3))
(define catch-var cadr)
(define catch-exps cddr)
(define finally? (tagged-list 'finally >= 2))
(define finally-exps cdr)

;;------------------------------------------------------------------------
;; file parser

;; for testing purposes
(define parse-file
  (lambda (filename)
    (scan-input (read-content filename) test-handler
      (lambda (tokens)
	(print-parsed-sexps tokens test-handler)))))

;; for testing purposes
(define print-parsed-sexps
  (lambda (tokens handler)
    (if (token-type? (first tokens) 'end-marker)
      'done
      (read-sexp tokens handler
	(lambda (datum tokens-left)
	  (parse datum handler
	    (lambda (exp)
	      (pretty-print exp)
	      (print-parsed-sexps tokens-left handler))))))))
