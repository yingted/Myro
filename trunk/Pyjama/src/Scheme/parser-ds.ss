;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader.ss")
(load "unifier-ds.ss")

(define apply-cont
  (lambda (k value)
    (case (car k)
       (parser (apply-parser-cont k value))
       (unifier (apply-unifier-cont k value))
       (reader
	 (set! k_reg k)
	 (set! sexp_reg value)
	 (set! pc apply-reader-cont)
	 (run))
       (else (error 'apply-cont "invalid continuation type: '~s'" (car k))))))

(define apply-parser-cont
  (lambda (k value)
    (record-case (cdr k)
       (init () value)
       (print-parsed-sexps (tokens-left handler)
	   (pretty-print value)
	   (print-parsed-sexps tokens-left handler))
       (expand-quasi-1 (v1 v2 k)
	   (apply-cont k `(cons ,v1 ,value)))
       (expand-quasi-2 (datum handler v2 k)
	   (expand-quasiquote (cdr datum) handler (make-cont 'parser 'expand-quasi-1 value v2 k)))
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
       (parse-4 (datum k body cvar cexps)
           (let ((cvar (catch-var (caddr datum))))
	     (apply-cont k (try-catch-finally-exp body cvar cexps value))))
       (parse-5 (datum handler k body cvar)
           (parse-all (finally-exps (cadddr datum)) handler (make-cont 'parser 'parse-4 datum k body cvar value)))
       (parse-6 (datum handler k cvar)
           (parse-all (catch-exps (caddr datum)) handler (make-cont 'parser 'parse-5 datum handler k value cvar)))
       (parse-7 (k body)
	   (apply-cont k (try-finally-exp body value)))
       (parse-8 (datum handler k)
	   (parse-all (finally-exps (caddr datum)) handler (make-cont 'parser 'parse-7 k value)))
       (parse-9 (datum k body cvar)
	   (let ((cvar (catch-var (caddr datum))))
	     (apply-cont k (try-catch-exp body cvar value))))
       (parse-10 (datum handler datum k cvar)
	   (parse-all (catch-exps (caddr datum)) handler (make-cont 'parser 'parse-9 datum k value cvar)))
       (parse-11 (datum)
	   (if (proper-list? (cadr datum))
	     (apply-cont k (lambda-exp (cadr datum) value))
	     (apply-cont k (mu-lambda-exp (head (cadr datum)) (last (cadr datum)) value))))
       (parse-12 (k)
	   (apply-cont k (begin-exp value)))
       (parse-13 (k datum)
	   (apply-cont k (define-exp (cadr datum) value)))
       (parse-14 (k datum)
	   (apply-cont k (assign-exp (cadr datum) value)))
       (parse-15 (k v1 v2)
	   (apply-cont k (if-exp v1 v2 value)))
       (parse-16 (datum handler k v1)
	   (parse (cadddr datum) handler (make-cont 'parser 'parse-15 k v1 value)))
       (parse-17 (datum handler k)
	   (parse (caddr datum) handler (make-cont 'parser 'parse-16 datum handler k value)))
       (parse-18 (k v1)
	   (apply-cont k (if-exp v1 value (lit-exp #f))))
       (parse-19 (datum handler k)
	   (parse (caddr datum) handler (make-cont 'parser 'parse-18 k value)))
       (parse-20 (handler k)
	   (parse value handler k))
       (parse-21 (handler k)
	   (parse value handler k))
       (process-macro-clauses (right-pattern k clauses datum handler)
           (if value
	     (instantiate right-pattern value k)
	     (process-macro-clauses (cdr clauses) datum handler k)))
       (expand-once (datum handler k datum)
	   (if (list? value)
	     (process-macro-clauses value datum handler k)
	     (apply-cont k (value datum))))
       (lookup-cont (k)
	 (apply-cont k (binding-value binding)))
       (else (error 'apply-parser-cont "invalid continuation: '~s'" k)))))

(define apply-cont2
  (lambda (k datum tokens-left)
    (record-case k
       (print-parsed-sexps-2 (handler)
	   (parse datum handler (make-cont 'parser 'print-parsed-sexps tokens-left handler)))
       (parse-string (handler)
	   (parse datum handler (make-cont 'parser 'init)))
       (else (error 'apply-cont2 "invalid continuation: '~s'" k)))))

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
    (lookup-value (car datum) macro-env handler (make-cont 'parser 'expand-once datum handler k datum))))

(define process-macro-clauses
  (lambda (clauses datum handler k)
    (if (null? clauses)
      (parser-apply-handler handler (format "no matching clause found for ~s" datum))
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses)))
	(unify-patterns left-pattern datum (make-cont 'parser 'process-macro-clauses right-pattern k clauses datum handler))))))

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
(define parse-string
  (lambda (string)
    (parse (read-string string) test-handler (make-cont 'parser 'init))))
;;    (read-datum string test-handler (make-cont 'parser 'parse-string test-handler))))

(define parse
  (lambda (datum handler k)
    (cond
      ((literal? datum) (apply-cont k (lit-exp datum)))
      ((quote? datum) (apply-cont k (lit-exp (cadr datum))))
      ((quasiquote? datum)
       (expand-quasiquote (cadr datum) handler (make-cont 'parser 'parse-21 handler k)))
      ((unquote? datum) (parser-apply-handler handler (format "misplaced ~s" datum)))
      ((unquote-splicing? datum) (parser-apply-handler handler (format "misplaced ~s" datum)))
      ((symbol? datum) (apply-cont k (var-exp datum)))
      ((syntactic-sugar? datum)
       (expand-once datum handler (make-cont 'parser 'parse-20 handler k)))
      ((if-then? datum)
       (parse (cadr datum) handler (amke-cont 'parse-19 datum handler k))) 
      ((if-else? datum)
       (parse (cadr datum) handler (make-cont 'parser 'parse-17 datum handler k)))
      ((assignment? datum)
       (parse (caddr datum) handler (make-cont 'parser 'parse-14 k datum)))
      ((define? datum)
       (if (mit-define? datum)
	 (parse (mit-define-transformer datum) handler k)
	 (parse (caddr datum) handler (make-cont 'parser 'parse-13 k datum))))
      ((define-syntax? datum)
       (apply-cont k (define-syntax-exp (cadr datum) (cddr datum))))
      ((begin? datum)
       (parse-all (cdr datum) handler (make-cont 'parser 'parse-12 k)))
      ((lambda? datum)
       (parse (cons 'begin (cddr datum)) handler (make-cont 'parser 'parse-11 datum)))
      ((try? datum)
       (cond
	 ((= (length datum) 2)
	  ;; (try <body>)
	  (parse (try-body datum) handler k))
	 ((and (= (length datum) 3) (catch? (caddr datum)))
	  ;; (try <body> (catch <var> <exp> ...))
	  (parse (try-body datum) handler (make-cont 'parser 'parse-10 datum handler datum k cvar)))
	 ((and (= (length datum) 3) (finally? (caddr datum)))
	  ;; (try <body> (finally <exp> ...))
	  (parse (try-body datum) handler (make-cont 'parser 'parse-8 datum handler k)))
	 ((and (= (length datum) 4) (catch? (caddr datum)) (finally? (cadddr datum)))
	  ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	  (parse (try-body datum) handler (make-cont 'parser 'parse-6 datum handler k cvar)))
	 (else (parser-apply-handler handler (format "bad try syntax: ~s" datum)))))
      ((raise? datum)
       (parse (cadr datum) handler (make-cont 'parser 'parse-3 k)))
      ((application? datum)
       (parse (car datum) handler (make-cont 'parser 'parse-2 datum handler k)))
      (else (parser-apply-handler handler (format "bad concrete syntax: ~s" datum))))))

(define parse-all
  (lambda (datum-list handler k)
    (if (null? datum-list)
	(apply-cont k '())
	(parse (car datum-list) handler (make-cont 'parser 'parse-all-2 datum-list handler k)))))

(define expand-quasiquote
  (lambda (datum handler k)
    (cond
      ((vector? datum)
       (expand-quasiquote (vector->list datum) handler (make-cont 'parser 'expand-quasi-4 k)))
      ((not (pair? datum)) (apply-cont k `(quote ,datum)))
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) (apply-cont k `(quote ,datum)))
      ((unquote? datum) (apply-cont k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
	 (apply-cont k (cadr (car datum)))
	 (expand-quasiquote (cdr datum) handler (make-cont 'parser 'expand-quasi-3 datum k))))
      (else
       (expand-quasiquote (car datum) handler (make-cont 'parser 'expand-quasi-2 datum handler v2 k))))))

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
(define define? (tagged-list 'define = 3))
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
    (print-parsed-sexps (scan-file filename) test-handler)))

;; may need fixing

;; for testing purposes
(define print-parsed-sexps
  (lambda (tokens handler)
    (if (token-type? (first tokens) 'end-marker)
      'done
      (begin
	(set! tokens_reg tokens)
	(set! handler_reg test-handler)
	(set! k_reg (make-cont 'parser 'print-parsed-sexps-2 handler))
	(set! pc read-sexp)
	(run)))))

;;      (read-sexp tokens handler (make-cont 'parser 'print-parsed-sexps-2 handler)))))

;; temporary
(define parser-apply-handler
  (lambda (handler exception)
    (set! handler_reg handler)
    (set! exception_reg exception)
    (set! pc apply-handler)
    (run)))
