;; Interpreter

(load "environments-ds.ss")
(load "parser-ds.ss")

;; temporary
(define interp-apply-handler
  (lambda (handler exception)
    (set! handler_reg handler)
    (set! exception_reg exception)
    (apply-handler)))

;;----------------------------------------------------------------------------
;; procedures represented as data structures

(define make-procedure
  (lambda args
    (cons 'procedure args)))

(define make-primitive
  (lambda (name)
    (let ((tag (string->symbol (string-append (symbol->string name) "-prim"))))
      (list 'procedure tag name))))

(define procedure-rep?
  (lambda (x)
    (and (list? x) (not (null? x)) (eq? (car x) 'procedure))))

(define safe-print
  (lambda (x)
    (if (procedure-rep? x)
      (case (cadr x)
	(closure (printf "#[procedure]~%"))
	(mu-closure (printf "#[procedure]~%"))
	(fake-k (printf "#[continuation]~%"))
	(else (printf "#[primitive ~a]~%" (caddr x))))
      ;; temporary: we'll need to fully implement safe-print
      (pretty-print x))))

(define apply-proc
  (lambda (proc args env handler k)
    (record-case (cdr proc)
      (closure (formals body env)
	(if (= (length args) (length formals))
	  (m body (extend env formals args) handler k)
	  (interp-apply-handler handler "incorrect number of arguments")))
      (mu-closure (formals runt body env)
	(if (>= (length args) (length formals))
	    (let ((new-env
		   (extend env
		     (cons runt formals)
		     (cons (list-tail args (length formals))
			   (list-head args (length formals))))))
	      (m body new-env handler k))
	    (interp-apply-handler handler "not enough arguments given")))
      (fake-k (k) (apply-cont k (car args)))
      (exit-prim ()
	(set! macro-env (make-macro-env))
	(set! toplevel-env (make-toplevel-env))
	;; temporary
	(set! load-stack '())
	'(exiting the interpreter))
      (sqrt-prim (name) (apply-cont k (apply sqrt args)))
      (print-prim (name) (begin (for-each safe-print args) (apply-cont k 'ok)))
      (display-prim (name) (apply display args))
      (newline-prim (name) (begin (newline) (apply-cont k 'ok)))
      (load-prim (name) (load-file-temp (car args) toplevel-env handler k))
      (null?-prim (name) (apply-cont k (apply null? args)))
      (cons-prim (name) (apply-cont k (apply cons args)))
      (car-prim (name) (apply-cont k (apply car args)))
      (cdr-prim (name) (apply-cont k (apply cdr args)))
      (list-prim (name) (apply-cont k args))
      (+-prim (name) (apply-cont k (apply + args)))
      (--prim (name) (apply-cont k (apply - args)))
      (*-prim (name) (apply-cont k (apply * args)))
      (/-prim (name) (apply-cont k (apply / args)))
      (<-prim (name) (apply-cont k (apply < args)))
      (>-prim (name) (apply-cont k (apply > args)))
      (=-prim (name) (apply-cont k (apply = args)))
      (equal?-prim (name) (apply-cont k (apply equal? args)))
      (range-prim (name) (apply-cont k (apply range args)))
      (set-car!-prim (name) (apply-cont k (apply set-car! args)))
      (set-cdr!-prim (name) (apply-cont k (apply set-cdr! args)))
      (import-prim (name) (import-primitive args env handler k))
      (get-prim (name) (get-primitive args env handler k))
      (call-with-current-continuation-prim (name)
	(call/cc-primitive (car args) env handler k))
      (call/cc-prim (name) (call/cc-primitive (car args) env handler k))
      (reverse-prim (name) (apply-cont k (apply reverse args)))
      (append-prim (name) (apply-cont k (apply append args)))
      (list->vector-prim (name) (apply-cont k (apply list->vector args)))
      (dir-prim (name) (apply-cont k (get-variables env)))
      (env-prim (name) (apply-cont k env))
      (current-time-prim (name)
	(apply-cont k (let ((now (current-time)))
			(+ (time-second now)
			   (inexact (/ (time-nanosecond now) 1000000000))))))
      (else (error 'apply-proc "bad procedure: ~a" proc)))))

;;----------------------------------------------------------------------------
;; continuations represented as data structures

(define apply-interpreter-cont
  (lambda (k value)
    (record-case (cdr k)
       (init () value)
       (rep-1 ()
	 (m value toplevel-env REP-handler REP-k))
       (REP-k ()
	 (safe-print value)
	 (read-eval-print-temp))
       (load-cont-1 (k)
	 (set! load-stack (cdr load-stack))
	 (apply-cont k value))
       (load-cont-2 (tokens-left env handler k)
	 (load-loop-temp tokens-left env handler k))
       (load-cont-3 (tokens-left env handler k)
	 (m value env handler
	   (make-cont 'interpreter 'load-cont-2 tokens-left env handler k)))
       (import-prim-cont (filename env handler k)
	 (let ((module (extend env '() '())))
	   (set-binding-value! value module)
	   (load-file-temp filename module handler k)))
       (get-prim-cont (args sym handler k)
	 (cond
	   ((null? (cdr args)) (apply-cont k value))
	   ((not (module? value))
	    (interp-apply-handler handler (format "~a is not a module" sym)))
	   (else (get-primitive (cdr args) value handler k))))
       (eval-sequence-cont (exps env handler k)
	 (if (null? (cdr exps))
	   (apply-cont k value)
	   (eval-sequence (cdr exps) env handler k)))
       (m*-1 (v1 k)
	 (apply-cont k (cons v1 value)))
       (m*-2 (exps env handler k)
	 (m* (cdr exps) env handler (make-cont 'interpreter 'm*-1 value k)))
       (try-finally-handler-cont (handler exception)
	 ;;(printf "propagating ~a exception~%" exception)
	 (interp-apply-handler handler exception))
       (m-1 (proc env handler k)
	 (apply-proc proc value env handler k))
       (m-2 (operands env handler k)
	 (m* operands env handler (make-cont 'interpreter 'm-1 value env handler k)))
       (m-3 (handler)
	 ;; todo: pass in more info to handler (k, env)
	 (interp-apply-handler handler value))
       (m-4 (v k)
	 (apply-cont k v))
       (m-5 (fexps env handler k)
	 ;;(printf "executing finally block~%")
	 (eval-sequence fexps env handler (make-cont 'interpreter 'm-4 value k)))
       (m-11 (clauses k)
	 (set-binding-value! value clauses)
	 (apply-cont k 'ok))
       (m-12 (rhs-value k)
	 (set-binding-value! value rhs-value)
	 (apply-cont k 'ok))
       (m-13 (var env handler k)
	 (lookup-binding-in-first-frame var env handler
	   (make-cont 'interpreter 'm-12 value k)))
       (m-15 (var env handler k)
	 (lookup-binding var env handler (make-cont 'interpreter 'm-12 value k)))
       (m-16 (then-exp else-exp env handler k)
	 (if value
	   (m then-exp env handler k)
	   (m else-exp env handler k)))
       (split-var-cont (variable env handler k)
	 (if value
	   (lookup-variable-components value "" env handler k)
	   (interp-apply-handler handler (format "unbound variable ~a" variable))))
       (lookup-module-var-cont (components var path handler k)
	 (if (null? (cdr components))
	   (apply-cont k value)
	   (let ((result (binding-value value))
		 (new-path (if (string=? path "")
			     (format "~a" var)
			     (format "~a.~a" path var))))
	     (if (not (module? result))
	       (interp-apply-handler handler (format "~a is not a module" new-path))
	       (lookup-variable-components
		 (cdr components) new-path result handler k)))))
       (else (error 'apply-interpreter-cont "bad continuation: ~a" k)))))

(define start
  (lambda ()
    (set! load-stack '())
    (read-eval-print-temp)))

(define REP-k (make-cont 'interpreter 'REP-k))

(define REP-handler (make-handler 'REP-handler))

(define read-eval-print-temp
  (lambda ()
    (printf "==> ")
    (let ((input (read)))
      (cond
        ((eq? input 'quit)
	 ;; temporary
	 (set! macro-env (make-macro-env))
	 (set! toplevel-env (make-toplevel-env))
	 (set! load-stack '())
	 '(exiting the interpreter))
	((not (string? input))
	 (printf "give me a string~%")
	 (read-eval-print-temp))
	(else
	  (let ((datum (read-string input)))
	    (if (exception?-temp datum)
	      (interp-apply-handler REP-handler (cadr datum))
	      (parse datum REP-handler (make-cont 'interpreter 'rep-1)))))))))

(define m
  (lambda (exp env handler k)
    (cases expression exp
      (lit-exp (datum) (apply-cont k datum))
      (var-exp (id) (lookup-value id env handler k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler
	  (make-cont 'interpreter 'm-16 then-exp else-exp env handler k)))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler (make-cont 'interpreter 'm-15 var env handler k))) 
      (define-exp (var rhs-exp)
	(m rhs-exp env handler (make-cont 'interpreter 'm-13 var env handler k)))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler
	  (make-cont 'interpreter 'm-11 clauses k)))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp (formals body)
	(apply-cont k (closure formals body env)))
      (mu-lambda-exp (formals runt body)
	(apply-cont k (mu-closure formals runt body env)))
      (try-catch-exp (body cvar cexps)
	(let ((new-handler (make-handler 'try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler k)))
      (try-finally-exp (body fexps)
	(let ((new-handler (make-handler 'try-finally-handler fexps env handler)))
	  (m body env new-handler (make-cont 'interpreter 'm-5 fexps env handler k)))) 
      (try-catch-finally-exp (body cvar cexps fexps)
	(let ((new-handler
		(make-handler 'try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler (make-cont 'interpreter 'm-5 fexps env handler k))))
      (raise-exp (exp)
	(m exp env handler (make-cont 'interpreter 'm-3 handler)))
      (app-exp (operator operands)
	(m operator env handler (make-cont 'interpreter 'm-2 operands env handler k)))
      (else (error 'm "bad abstract syntax: ~a" exp)))))

(define closure
  (lambda (formals body env)
    (make-procedure 'closure formals body env)))

(define mu-closure
  (lambda (formals runt body env)
    (make-procedure 'mu-closure formals runt body env)))

(define m*
  (lambda (exps env handler k)
    (if (null? exps)
      (apply-cont k '())
      (m (car exps) env handler (make-cont 'interpreter 'm*-2 exps env handler k)))))

(define eval-sequence
  (lambda (exps env handler k)
    (m (car exps) env handler
      (make-cont 'interpreter 'eval-sequence-cont exps env handler k))))

(define make-toplevel-env
  (lambda ()
    (make-initial-environment
      (list 'nil 'exit 'sqrt 'print 'display 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'equal? 'range 'set-car! 'set-cdr!
	    'import 'get 'call-with-current-continuation 'call/cc
	    'reverse 'append 'list->vector 'dir 'env 'current-time)
      (list '()
	    (make-primitive 'exit)
	    (make-primitive 'sqrt)
	    (make-primitive 'print)
	    (make-primitive 'display)
	    (make-primitive 'newline)
	    (make-primitive 'load)
	    (make-primitive 'null?)
	    (make-primitive 'cons)
	    (make-primitive 'car)
	    (make-primitive 'cdr)
	    (make-primitive 'list)
	    (make-primitive '+)
	    (make-primitive '-)
	    (make-primitive '*)
	    (make-primitive '/)
	    (make-primitive '<)
	    (make-primitive '>)
	    (make-primitive '=)
	    (make-primitive 'equal?)
	    (make-primitive 'range)
	    (make-primitive 'set-car!)
	    (make-primitive 'set-cdr!)
	    (make-primitive 'import)
	    (make-primitive 'get)
	    (make-primitive 'call-with-current-continuation)
	    (make-primitive 'call/cc)
	    (make-primitive 'reverse)
	    (make-primitive 'append)
	    (make-primitive 'list->vector)
	    (make-primitive 'dir)
	    (make-primitive 'env)
	    (make-primitive 'current-time)))))	    

(define get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value sym env handler
	(make-cont 'interpreter 'get-prim-cont args sym handler k)))))

;; need a more reliable test for a module/environment
(define module?
  (lambda (x)
    (and (list? x) (not (null? x)) (list? (car x)))))

(define import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file-temp filename env handler k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler
	      (make-cont 'interpreter 'import-prim-cont filename env handler k)))))))

(define call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (make-procedure 'fake-k k)))
      (apply-proc proc (list fake-k) env handler k))))

(define get-variables
  (lambda (env)
    (map (lambda (frame) (map binding-variable frame)) env)))

(define load-stack '())

;; temporary
(define exception?-temp
  (lambda (x)
    (and (list? x)
	 (not (null? x))
	 (eq? (car x) 'exception))))

;; temporary
(define load-file-temp
  (lambda (filename env handler k)
    ;;(printf "calling load-file-temp~%")
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (apply-cont k 'ok))
      ((not (string? filename))
       (interp-apply-handler handler (format "filename is not a string: ~a" filename)))
      ((not (file-exists? filename))
       (interp-apply-handler handler (format "file does not exist: ~a" filename)))
      (else
	(set! load-stack (cons filename load-stack))
	(let ((result (scan-string (read-content filename))))
	  (if (exception?-temp result)
	    (interp-apply-handler handler (cadr result))
	    (let ((tokens result))
	      (load-loop-temp tokens env handler
		(make-cont 'interpreter 'load-cont-1 k)))))))))

;; temporary
(define load-loop-temp
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
      (apply-cont k 'ok)
      (let ((result (read-next-sexp tokens)))
	(if (exception?-temp result)
	  (interp-apply-handler handler (cadr result))
	  (let ((datum (car result))
		(tokens-left (cdr result)))
	    ;;(printf "read ~a~%" datum)
	    (parse datum handler
	      (make-cont 'interpreter 'load-cont-3 tokens-left env handler k))))))))

(define range
  (lambda args
    (letrec
	((range
	  (lambda (n end step acc)
	    (if (>= n end)
	      (reverse acc)
	      (range (+ n step) end step (cons n acc))))))
      (cond
	((null? (cdr args)) (range 0 (car args) 1 '()))
	((null? (cddr args)) (range (car args) (cadr args) 1 '()))
	(else (range (car args) (cadr args) (caddr args) '()))))))
	
(define toplevel-env (make-toplevel-env))
(define macro-env (make-macro-env))
