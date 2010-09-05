;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader.ss")
(load "unifier-ds.ss")

(define apply-cont
  (lambda (k value)
    (case (car k)
       (interpreter (apply-interpreter-cont k value))
       (parser (apply-parser-cont k value))
       (unifier (apply-unifier-cont k value))
       (reader
	 (set! k_reg k)
	 (set! sexp_reg value)
	 (set! pc apply-reader-cont)
	 (run))
       (else (error 'apply-cont "bad continuation type: ~a" (car k))))))

(define apply-parser-cont
  (lambda (k value)
    (record-case (cdr k)
       (init () value)
       (expand-quasi-1 (v1 k)
	   (apply-cont k `(cons ,v1 ,value)))
       (expand-quasi-2 (datum handler k)
	   (expand-quasiquote (cdr datum) handler
	     (make-cont 'parser 'expand-quasi-1 value k)))
       (expand-quasi-3 (datum k)
	   (apply-cont k `(append ,(cadr (car datum)) ,value)))
       (expand-quasi-4 (k)
	   (apply-cont k `(list->vector ,value)))
       (parse-all-1 (a k)
           (apply-cont k (cons a value)))
       (parse-all-2 (datum-list handler k)
   	   (parse-all (cdr datum-list) handler (make-cont 'parser 'parse-all-1 value k)))
       (parse-1 (v1 k)
           (apply-cont k (app-exp v1 value)))
       (parse-2 (datum handler k)
	   (parse-all (cdr datum) handler (make-cont 'parser 'parse-1 value k)))
       (parse-3 (k)
	   (apply-cont k (raise-exp value)))
       (parse-4 (datum k body cexps)
           (let ((cvar (catch-var (caddr datum))))
	     (apply-cont k (try-catch-finally-exp body cvar cexps value))))
       (parse-5 (datum handler k body)
           (parse-all (finally-exps (cadddr datum)) handler
	     (make-cont 'parser 'parse-4 datum k body value)))
       (parse-6 (datum handler k)
           (parse-all (catch-exps (caddr datum)) handler
	     (make-cont 'parser 'parse-5 datum handler k value)))
       (parse-7 (k body)
	   (apply-cont k (try-finally-exp body value)))
       (parse-8 (datum handler k)
	   (parse-all (finally-exps (caddr datum)) handler
	     (make-cont 'parser 'parse-7 k value)))
       (parse-9 (datum k body)
	   (let ((cvar (catch-var (caddr datum))))
	     (apply-cont k (try-catch-exp body cvar value))))
       (parse-10 (datum handler k)
	   (parse-all (catch-exps (caddr datum)) handler
	     (make-cont 'parser 'parse-9 datum k value)))
       (parse-11 (datum k)
	   (if (list? (cadr datum))
	     (apply-cont k (lambda-exp (cadr datum) value))
	     (apply-cont k (mu-lambda-exp (head (cadr datum)) (last (cadr datum)) value))))
       (parse-12 (datum handler k)
	   (cond
	     ((null? value) (apply-handler handler (format "bad concrete syntax: ~a" datum)))
	     ((null? (cdr value)) (apply-cont k (car value)))
	     (else (apply-cont k (begin-exp value)))))
       (parse-13 (k datum)
	   (apply-cont k (define-exp (cadr datum) value)))
       (parse-14 (k datum)
	   (apply-cont k (assign-exp (cadr datum) value)))
       (parse-15 (k v1 v2)
	   (apply-cont k (if-exp v1 v2 value)))
       (parse-16 (datum handler k v1)
	   (parse (cadddr datum) handler (make-cont 'parser 'parse-15 k v1 value)))
       (parse-17 (datum handler k)
	   (parse (caddr datum) handler
	     (make-cont 'parser 'parse-16 datum handler k value)))
       (parse-18 (k v1)
	   (apply-cont k (if-exp v1 value (lit-exp #f))))
       (parse-19 (datum handler k)
	   (parse (caddr datum) handler (make-cont 'parser 'parse-18 k value)))
       (parse-20 (handler k)
	   (parse value handler k))
       (parse-21 (handler k)
	   (parse value handler k))
       (process-macro-cont (right-pattern k clauses datum handler)
           (if value
	     (instantiate right-pattern value k)
	     (process-macro-clauses (cdr clauses) datum handler k)))
       (expand-once-cont (datum handler k)
	   (if (list? value)
	     (process-macro-clauses value datum handler k)
	     (apply-cont k (value datum))))
       (lookup-cont (k)
	 (apply-cont k (binding-value value)))
       (print-parsed-sexps-cont (tokens-left handler)
	 (pretty-print value)
	 (print-parsed-sexps tokens-left handler))
       (parse-sexps-cont (tokens-left handler k)
	 (parse-sexps tokens-left handler (make-cont 'parser 'parse-sexps-cont-2 value k)))
       (parse-sexps-cont-2 (previous k)
	 (apply-cont k (cons previous value)))
       (else (error 'apply-parser-cont "bad continuation: ~a" k)))))

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

(load "environments-ds.ss")

(define syntactic-sugar?
  (lambda (datum)
    (and (list? datum)
	 (not (null? datum))
	 (symbol? (car datum))
	 (search-env macro-env (car datum)))))

(define expand-once
  (lambda (datum handler k)
    (lookup-value (car datum) macro-env handler
      (make-cont 'parser 'expand-once-cont datum handler k))))

(define process-macro-clauses
  (lambda (clauses datum handler k)
    (if (null? clauses)
      (parser-apply-handler handler (format "no matching clause found for ~a" datum))
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses)))
	(unify-patterns left-pattern datum
	  (make-cont 'parser 'process-macro-cont right-pattern k clauses datum handler))))))

(define mit-define-transformer
  (lambda (datum)
    (let ((name (caadr datum))
	  (formals (cdadr datum))
	  (bodies (cddr datum)))
      `(define ,name (lambda ,formals ,@bodies)))))

(define and-transformer
  (lambda (datum)
    (let ((exps (cdr datum)))
      (cond
	((null? exps) '#t)
	((null? (cdr exps)) (car exps))
	(else `(if ,(car exps) (and ,@(cdr exps)) #f))))))

;; avoids variable capture
(define or-transformer
  (lambda (datum)
    (let ((exps (cdr datum)))
      (cond
	((null? exps) '#f)
	((null? (cdr exps)) (car exps))
	(else `(let ((bool ,(car exps))
		     (else-code (lambda () (or ,@(cdr exps)))))
		 (if bool bool (else-code))))))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer
  (lambda (datum)
    (let ((clauses (cdr datum)))
      (if (null? clauses)
	(error 'cond-transformer "bad concrete syntax: ~a" datum)
	(let ((first-clause (car clauses))
	      (other-clauses (cdr clauses)))
	  (if (or (null? first-clause) (not (list? first-clause)))
	    (error 'cond-transformer "bad concrete syntax: ~a" datum)
	    (let ((test-exp (car first-clause))
		  (then-exps (cdr first-clause)))
	      (cond
		((eq? test-exp 'else)
		 (if (null? then-exps)
		   (error 'cond-transformer "bad concrete syntax: (else)")
		   `(begin ,@then-exps)))
		((null? then-exps)
		 (if (null? other-clauses)
		   `(let ((bool ,test-exp))
		      (if bool bool))
		   `(let ((bool ,test-exp)
			  (else-code (lambda () (cond ,@other-clauses))))
		      (if bool bool (else-code)))))
		((null? other-clauses) `(if ,test-exp (begin ,@then-exps)))
		(else `(if ,test-exp (begin ,@then-exps) (cond ,@other-clauses)))))))))))

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

(define let*-transformer
  (lambda (datum)
    (let ((bindings (cadr datum))
	  (bodies (cddr datum)))
      (letrec
	((nest
	   (lambda (bindings)
	     (if (or (null? bindings) (null? (cdr bindings)))
	       `(let ,bindings ,@bodies)
	       `(let (,(car bindings)) ,(nest (cdr bindings)))))))
	(nest bindings)))))
	   
;; avoids variable capture
(define record-case-transformer
  (lambda (datum)
    (letrec
      ((rc-clause->cond-clause
	 (lambda (clause)
	   (let ((tag (if (symbol? (car clause)) (car clause) (caar clause))))
	     (cond
	       ((eq? (car clause) 'else) `(else (else-code)))
	       ((symbol? (car clause)) `((eq? (car r) ',tag) (apply ,tag (cdr r))))
	       (else `((memq (car r) ',(car clause)) (apply ,tag (cdr r))))))))
       (rc-clause->let-binding
	 (lambda (clause)
	   (let ((tag (if (symbol? (car clause)) (car clause) (caar clause))))
	     (if (eq? tag 'else)
	       `(else-code (lambda () ,@(cdr clause)))
	       `(,tag (lambda ,(cadr clause) ,@(cddr clause))))))))
      (let ((exp (cadr datum))
	    (clauses (cddr datum)))
	`(let ((r ,exp) ,@(map rc-clause->let-binding clauses))
	   (cond ,@(map rc-clause->cond-clause clauses)))))))

(define make-macro-env
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'record-case)
      (list and-transformer
	    or-transformer
	    cond-transformer
	    let-transformer
	    letrec-transformer
	    let*-transformer
	    record-case-transformer))))

;; macros as define-syntax patterns:
;;            '([(and) #t]
;;	      [(and ?exp) ?exp]
;;	      [(and ?exp . ?exp*) (if ?exp (and . ?exp*) #f)])
;;	    '([(or) #f]
;;	      [(or ?exp) ?exp]
	      ;; incorrect:
	      ;;[(or ?exp . ?exp*) (if ?exp #t (or . ?exp*))]
;;	      [(or ?exp . ?exp*)
;;	       (let ((first ?exp) (rest (lambda () (or . ?exp*))))
;;		 (if first first (rest)))])
	    ;; not quite correct:
;;	    '([(cond) #t]
;;	      [(cond (else . ?results)) (begin . ?results)]
;;	      [(cond (?test . ?results)) (if ?test (begin . ?results) #f)]
;;	      [(cond (?test . ?results) . ?rest)
;;	       (if ?test (begin . ?results) (cond . ?rest))])
;;	    '([(let* () . ?bodies) (begin . ?bodies)]
;;	      [(let* ((?var ?exp) . ?rest) . ?bodies)
;;	       (let ((?var ?exp)) (let* ?rest . ?bodies))])

(define macro-env (make-macro-env))

;;--------------------------------------------------------------------------

(define parse
  (lambda (datum handler k)
    (cond
      ((literal? datum)
       (apply-cont k (lit-exp datum)))
      ((quote? datum)
       (apply-cont k (lit-exp (cadr datum))))
      ((quasiquote? datum)
       (expand-quasiquote (cadr datum) handler (make-cont 'parser 'parse-21 handler k)))
      ((unquote? datum)
       (parser-apply-handler handler (format "misplaced ~a" datum)))
      ((unquote-splicing? datum)
       (parser-apply-handler handler (format "misplaced ~a" datum)))
      ((symbol? datum)
       (apply-cont k (var-exp datum)))
      ((syntactic-sugar? datum)
       (expand-once datum handler (make-cont 'parser 'parse-20 handler k)))
      ((if-then? datum)
       (parse (cadr datum) handler (make-cont 'parser 'parse-19 datum handler k))) 
      ((if-else? datum)
       (parse (cadr datum) handler (make-cont 'parser 'parse-17 datum handler k)))
      ((assignment? datum)
       (parse (caddr datum) handler (make-cont 'parser 'parse-14 k datum)))
      ((define? datum)
       (if (mit-style? datum)
	 (parse (mit-define-transformer datum) handler k)
	 (parse (caddr datum) handler (make-cont 'parser 'parse-13 k datum))))
      ((define-syntax? datum)
       (apply-cont k (define-syntax-exp (cadr datum) (cddr datum))))
      ((begin? datum)
       (parse-all (cdr datum) handler (make-cont 'parser 'parse-12 datum handler k)))
      ((lambda? datum)
       (parse (cons 'begin (cddr datum)) handler (make-cont 'parser 'parse-11 datum k)))
      ((try? datum)
       (cond
	 ((= (length datum) 2)
	  ;; (try <body>)
	  (parse (try-body datum) handler k))
	 ((and (= (length datum) 3) (catch? (caddr datum)))
	  ;; (try <body> (catch <var> <exp> ...))
	  (parse (try-body datum) handler (make-cont 'parser 'parse-10 datum handler k)))
	 ((and (= (length datum) 3) (finally? (caddr datum)))
	  ;; (try <body> (finally <exp> ...))
	  (parse (try-body datum) handler (make-cont 'parser 'parse-8 datum handler k)))
	 ((and (= (length datum) 4) (catch? (caddr datum)) (finally? (cadddr datum)))
	  ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	  (parse (try-body datum) handler (make-cont 'parser 'parse-6 datum handler k)))
	 (else (parser-apply-handler handler (format "bad try syntax: ~a" datum)))))
      ((raise? datum)
       (parse (cadr datum) handler (make-cont 'parser 'parse-3 k)))
      ((application? datum)
       (parse (car datum) handler (make-cont 'parser 'parse-2 datum handler k)))
      (else (parser-apply-handler handler (format "bad concrete syntax: ~a" datum))))))

(define parse-all
  (lambda (datum-list handler k)
    (if (null? datum-list)
	(apply-cont k '())
	(parse (car datum-list) handler
	  (make-cont 'parser 'parse-all-2 datum-list handler k)))))

(define expand-quasiquote
  (lambda (datum handler k)
    (cond
      ((vector? datum)
       (expand-quasiquote (vector->list datum) handler
	 (make-cont 'parser 'expand-quasi-4 k)))
      ((not (pair? datum)) (apply-cont k `(quote ,datum)))
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) (apply-cont k `(quote ,datum)))
      ((unquote? datum) (apply-cont k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
	 (apply-cont k (cadr (car datum)))
	 (expand-quasiquote (cdr datum) handler
	   (make-cont 'parser 'expand-quasi-3 datum k))))
      (else
       (expand-quasiquote (car datum) handler
	 (make-cont 'parser 'expand-quasi-2 datum handler k))))))

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

(define mit-style?
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
;; for testing purposes

;; for testing purposes
(define exception?
  (lambda (x)
    (and (list? x)
	 (not (null? x))
	 (eq? (car x) 'exception))))

;; for testing purposes
(define parse-string
  (lambda (string)
    (let ((sexp (read-string string)))
      (if (exception? sexp)
	sexp
	(parse sexp test-handler (make-cont 'parser 'init))))))

;; for testing purposes
(define print-parse-file
  (lambda (filename)
    (print-parsed-sexps (scan-file filename) test-handler)))

;; for testing purposes
(define print-parsed-sexps
  (lambda (tokens handler)
    (if (token-type? (first tokens) 'end-marker)
      'done
      (let ((result (read-next-sexp tokens)))
	(if (exception? result)
	  (parser-apply-handler handler (cadr result))
	  (let ((sexp (car result))
		(tokens-left (cdr result)))
	    (parse sexp handler
	      (make-cont 'parser 'print-parsed-sexps-cont tokens-left handler))))))))

;; for testing purposes
(define parse-file
  (lambda (filename)
    (parse-sexps (scan-file filename) test-handler (make-cont 'parser 'init))))

;; for testing purposes
(define parse-sexps
  (lambda (tokens handler k)
    (if (token-type? (first tokens) 'end-marker)
      (apply-cont k '())
      (let ((result (read-next-sexp tokens)))
	(if (exception? result)
	  (parser-apply-handler handler (cadr result))
	  (let ((sexp (car result))
		(tokens-left (cdr result)))
	    (parse sexp handler
	      (make-cont 'parser 'parse-sexps-cont tokens-left handler k))))))))

;; temporary
(define parser-apply-handler
  (lambda (handler exception)
    (set! handler_reg handler)
    (set! exception_reg exception)
    (set! pc apply-handler)
    (run)))
