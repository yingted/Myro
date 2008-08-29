(load "transformer-macros.ss")

;;----------------------------------------------------------------------------
;; Interpreter

(load "environments-cps.ss")
(load "parser-cps.ss")

;; temporary
(define testall
  (lambda ()
    (read-datum "(load \"examples.ss\")" REP-handler
      (lambda-cont2 (datum tokens-left)
	(parse datum REP-handler
	  (lambda-cont (exp)
	    (m exp toplevel-env REP-handler
	      (lambda-cont (v)
		(read-datum "(test-all)" REP-handler
		  (lambda-cont2 (datum tokens-left)
		    (parse datum REP-handler
		      (lambda-cont (exp)
			(m exp toplevel-env REP-handler
			  (lambda-cont (v)
			    (read-datum "(exit)" REP-handler
			      (lambda-cont2 (datum tokens-left)
				(parse datum REP-handler
				  (lambda-cont (exp)
				    (m exp toplevel-env REP-handler REP-k)))))))))))))))))))

(define start
  (lambda ()
    (read-eval-print)))

(define *need-newline* #f)

(define REP-k
  (lambda-cont (v)
    (if (not (eq? v '<void>))
	(pretty-print-prim v))
    (if *need-newline* (newline))
    (read-eval-print)))

(define pretty-print-prim
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print arg)))

(define newline-prim
  (lambda ()
    (set! *need-newline* #f)
    (newline)))

(define display-prim
  (lambda (arg)
    (let* ((s (format "~s" arg))
	   (len (string-length s)))
      (set! *need-newline* (true? (not (equal? (substring s (- len 1) len) "\n"))))
      (display s))))

(define REP-handler
  (lambda-handler (e)
    (REP-k `(uncaught exception: ,e))))

(define* read-eval-print
  (lambda ()
    (printf "==> ")
    (let* ((input (read))
	   (input-string (format "~s" input)))
      (read-datum input-string REP-handler
	(lambda-cont2 (datum tokens-left)
	  (parse datum REP-handler
	    (lambda-cont (exp)
	      (m exp toplevel-env REP-handler REP-k))))))))

(define* m
  (lambda (exp env handler k)
    (cases expression exp
      (lit-exp (datum) (k datum))
      (var-exp (id) (lookup-value id env handler k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler
	  (lambda-cont (bool)
	    (if bool
	      (m then-exp env handler k)
	      (m else-exp env handler k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda-cont (rhs-value)
	    (lookup-binding var env handler
	      (lambda-cont (binding)
		(set-binding-value! binding rhs-value)
		(k '<void>))))))
      (define-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda-cont (rhs-value)
	    (lookup-binding-in-first-frame var env handler
	      (lambda-cont (binding)
		(set-binding-value! binding rhs-value)
		(k '<void>))))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler
	  (lambda-cont (binding)
	    (set-binding-value! binding (make-pattern-macro clauses))
	    (k '<void>))))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp (formals body)
	(k (closure formals body env)))
      (mu-lambda-exp (formals runt body)
	(k (mu-closure formals runt body env)))
      (try-catch-exp (body cvar cexps)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler k)))
      (try-finally-exp (body fexps)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler
	    (lambda-cont (v)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler
		(lambda-cont (v2) (k v)))))))
      (try-catch-finally-exp (body cvar cexps fexps)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler
	     (lambda-cont (v)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler
		 (lambda-cont (v2) (k v)))))))
      (raise-exp (exp)
	(m exp env handler
	  ;; todo: pass in more info to handler (k, env)
	  (lambda-cont (e) (handler e))))
      (app-exp (operator operands)
	(m operator env handler
	  (lambda-cont (proc)
	    (m* operands env handler
	      (lambda-cont (args)
		(proc args env handler k))))))
      (else (error 'm "bad abstract syntax: ~a" exp)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (lambda-handler (e)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e))))
	;;(printf "executing catch block~%")
	(eval-sequence cexps new-env handler k)))))

(define try-finally-handler
  (lambda (fexps env handler)
    (lambda-handler (e)
      ;;(printf "executing finally block~%")
      (eval-sequence fexps env handler
	(lambda-cont (v)
	  ;;(printf "propagating ~a exception~%" e)
	  (handler e))))))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (lambda-handler (e)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e))))
	(let ((catch-handler (try-finally-handler fexps env handler)))
	  ;;(printf "executing catch block~%")
	  (eval-sequence cexps new-env catch-handler
	    (lambda-cont (v)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler
		(lambda-cont (v2) (k v))))))))))

(define closure
  (lambda (formals body env)
    (lambda-proc (args env2 handler k2)
      (if (= (length args) (length formals))
	(m body (extend env formals args) handler k2)
	(handler "incorrect number of arguments")))))

(define mu-closure
  (lambda (formals runt body env)
    (lambda-proc (args env2 handler k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (m body new-env handler k2))
	(handler "not enough arguments given")))))

(define* m*
  (lambda (exps env handler k)
    (if (null? exps)
      (k '())
      (m (car exps) env handler
	(lambda-cont (v1)
	  (m* (cdr exps) env handler
	    (lambda-cont (v2)
	      (k (cons v1 v2)))))))))

(define* eval-sequence
  (lambda (exps env handler k)
    (m (car exps) env handler
       (lambda-cont (result)
	 (if (null? (cdr exps))
	   (k result)
	   (eval-sequence (cdr exps) env handler k))))))

(define make-initial-env-extended
  (lambda (env)
    ;; this is here as a hook for extending environments in C# etc.
    env))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
     (make-initial-environment
      (list 'exit 'apply 'sqrt 'print 'display 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'equal? 'eq? 'memq 'range 'set-car! 'set-cdr!
	    'import 'get 'call-with-current-continuation 'call/cc
	    'reverse 'append 'list->vector 'dir 'current-time 'map 'env
	    'using)
      (list
	;; exit
        (lambda-proc (args env2 handler k2)
	  (set! macro-env (make-macro-env))
	  (set! toplevel-env (make-toplevel-env))
	  ;; temporary
	  (set! load-stack '())
	  (halt* '(exiting the interpreter)))
	;; apply
	(lambda-proc (args env2 handler k2)
	  (let ((proc (car args))
		(proc-args (cadr args)))
	    (proc proc-args env2 handler k2)))
	;; sqrt
	(lambda-proc (args env2 handler k2) (k2 (apply sqrt args)))
	;; print
	(lambda-proc (args env2 handler k2) (for-each pretty-print-prim args) (k2 '<void>))
	;; display
	(lambda-proc (args env2 handler k2) (apply display-prim args) (k2 '<void>))
	;; newline
	(lambda-proc (args env2 handler k2) (newline-prim) (k2 '<void>))
	;; load
	(lambda-proc (args env2 handler k2) (load-file (car args) toplevel-env handler k2))
	;; null?
	(lambda-proc (args env2 handler k2) (k2 (apply null? args)))
	;; cons
	(lambda-proc (args env2 handler k2) (k2 (apply cons args)))
	;; car
	(lambda-proc (args env2 handler k2) (k2 (apply car args)))
	;; cdr
	(lambda-proc (args env2 handler k2) (k2 (apply cdr args)))
	;; list
	(lambda-proc (args env2 handler k2) (k2 args))
	;; +
	(lambda-proc (args env2 handler k2) (k2 (apply + args)))
	;; - 
	(lambda-proc (args env2 handler k2) (k2 (apply - args)))
	;; *
	(lambda-proc (args env2 handler k2) (k2 (apply * args)))
	;; /
	(lambda-proc (args env2 handler k2) (k2 (apply / args)))
	;; <
	(lambda-proc (args env2 handler k2) (k2 (apply < args)))
	;; >
	(lambda-proc (args env2 handler k2) (k2 (apply > args)))
	;; =
	(lambda-proc (args env2 handler k2) (k2 (apply = args)))
	;; equal?
	(lambda-proc (args env2 handler k2) (k2 (apply equal? args)))
	;; eq?
	(lambda-proc (args env2 handler k2) (k2 (apply eq? args)))
	;; memq
	(lambda-proc (args env2 handler k2) (k2 (apply memq args)))
	;; range
	(lambda-proc (args env2 handler k2) (k2 (apply range args)))
	;; set-car!
	(lambda-proc (args env2 handler k2) (k2 (apply set-car! args)))
	;; set-cdr
	(lambda-proc (args env2 handler k2) (k2 (apply set-cdr! args)))
	;; import
	(lambda-proc (args env2 handler k2) (import-primitive args env2 handler k2))
	;; get
	(lambda-proc (args env2 handler k2) (get-primitive args env2 handler k2))
	;; call/cc
	(lambda-proc (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	;; call/cc
	(lambda-proc (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	;; reverse
	(lambda-proc (args env2 handler k2) (k2 (apply reverse args)))
	;; append
	(lambda-proc (args env2 handler k2) (k2 (apply append args)))
	;; list->vector
	(lambda-proc (args env2 handler k2) (k2 (apply list->vector args)))
	;; dir
	(lambda-proc (args env2 handler k2) (k2 (dir args env2)))
	;; current-time
	(lambda-proc (args env2 handler k2) (k2 (get-current-time)))
	;; map
	(lambda-proc (args env2 handler k2)
	  (let ((proc (car args))
		(proc-args (cadr args)))
	    (map-prim proc proc-args env2 handler k2)))
	;; env
	(lambda-proc (args env2 handler k2) (k2 env2))
	;; using (not defined in scheme-scheme)
	(lambda-proc (args env2 handler k2) (k2 (using-prim args env2)))
	)))))

(define* map-prim
  (lambda (proc args env handler k)
    (if (null? args)
      (k '())
      (if (not (list? (car args)))
	(proc (list (car args)) env handler
	  (lambda-cont (v1)
	    (map-prim proc (cdr args) env handler
	      (lambda-cont (v2)
		(k (cons v1 v2))))))
	(proc (car args) env handler 
	  (lambda-cont (v1)
	    (map-prim proc (cdr args) env handler 
	      (lambda-cont (v2)
		(k (cons v1 v2))))))))))
		     
(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now)
		     1000000000))))))

(define* get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value sym env handler
	(lambda-cont (v)
	  (cond
	    ((null? (cdr args)) (k v))
	    ((not (environment? v)) (handler (format "~a is not a module" sym)))
	    (else (get-primitive (cdr args) v handler k))))))))

;; bug fix needed:
;; (import "my-fact.ss" 'm)
;; (m.m.m.m.m.fib 10) =>  89

(define* import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file filename env handler k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler
	      (lambda-cont (binding)
		(let ((module (extend env '() '())))
		  (set-binding-value! binding module)
		  (load-file filename module handler k)))))))))

(define* call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (lambda-proc (args env2 handler k2) (k (car args)))))
      (proc (list fake-k) env handler k))))

(define dir
  (lambda (args env)
     (if (null? args)
 	(map get-variables-from-frame (frames env))
	(car (map get-variables-from-frame (car args))))))

(define get-variables-from-frame
  (lambda (frame) 
    (sort symbol<? (map binding-variable frame))))

(define symbol<?
  (lambda (a b)
    (let ((a_string (symbol->string a))
	  (b_string (symbol->string b)))
      (string<? a_string b_string))))

(define load-stack '())

(define* load-file
  (lambda (filename env handler k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k '<void>))
      ((not (string? filename))
       (handler (format "filename is not a string: ~a" filename)))
      ((not (file-exists? filename))
       (handler (format "file does not exist: ~a" filename)))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) handler
	 (lambda-cont (tokens)
	   (load-loop tokens env handler
	     (lambda-cont (v)
	       (set! load-stack (cdr load-stack))
	       (k v)))))))))

(define* load-loop
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
      (k '<void>)
      (read-sexp tokens handler
	(lambda-cont2 (datum tokens-left)
	  (parse datum handler
	    (lambda-cont (exp)
	      (m exp env handler
		(lambda-cont (v)
		  (load-loop tokens-left env handler k))))))))))

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
	
(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 handler k2)
      (k2 (apply* external-function-object args)))))

(define toplevel-env (make-toplevel-env))
(define macro-env (make-macro-env))

