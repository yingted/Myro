(load "transformer-macros.ss")

;;----------------------------------------------------------------------------
;; Interpreter

(load "environments-cps.ss")
(load "parser-cps.ss")

(define start
  (lambda ()
    (read-eval-print-temp)))

(define REP-k
  (lambda-cont (v)
    (safe-print v)
    (read-eval-print-temp)))

(define REP-handler
  (lambda-handler (e)
    (REP-k `(uncaught exception: ,e))))

;; temporary version for use with non-registerized interpreter.  this
;; will be replaced by a fully registerized version of load-loop when
;; the interpreter is registerized.
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
	     (REP-handler (cadr datum))
	     (parse datum REP-handler
	       (lambda-cont (exp)
		 (m exp toplevel-env REP-handler REP-k))))))))))

(define read-eval-print
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
	 (read-eval-print))
	(else
	 (read-datum input REP-handler
	   (lambda-cont2 (datum tokens-left)
	     (parse datum REP-handler
	       (lambda-cont (exp)
		 (m exp toplevel-env REP-handler REP-k))))))))))

;; used by the data structure version of the code
(define data-structure-procedure?
  (lambda (x)
    (and (list? x)
	 (not (null? x))
	 (eq? (car x) 'procedure))))

;; we'll need to fully implement safe-print
(define safe-print
  (lambda (x)
    (if (data-structure-procedure? x)
      (printf "#[procedure]~%")
      (pretty-print x))))

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
		(k 'ok))))))
      (define-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda-cont (rhs-value)
	    (lookup-binding-in-first-frame var env handler
	      (lambda-cont (binding)
		(set-binding-value! binding rhs-value)
		(k 'ok))))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler
	  (lambda-cont (binding)
	    (set-binding-value! binding clauses)
	    (k 'ok))))
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

(define make-toplevel-env
  (lambda ()
    (make-initial-environment
      (list 'nil 'exit 'apply 'sqrt 'print 'display 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'equal? 'eq? 'range 'set-car! 'set-cdr!
	    'import 'get 'call-with-current-continuation 'call/cc
	    'reverse 'append 'list->vector 'dir 'env 'current-time)
      (list '()
	    (lambda-proc (args env2 handler k2)
	      (set! macro-env (make-macro-env))
	      (set! toplevel-env (make-toplevel-env))
	      ;; temporary
	      (set! load-stack '())
	      '(exiting the interpreter))
	    (lambda-proc (args env2 handler k2)
	      (let ((proc (car args))
		    (proc-args (cadr args)))
		(proc proc-args env2 handler k2)))
	    (lambda-proc (args env2 handler k2) (k2 (apply sqrt args)))
	    (lambda-proc (args env2 handler k2) (for-each safe-print args) (k2 'ok))
	    (lambda-proc (args env2 handler k2) (apply display args) (k2 'ok))
	    (lambda-proc (args env2 handler k2) (newline) (k2 'ok))
	    ;; temporary
	    (lambda-proc (args env2 handler k2) (load-file-temp (car args) toplevel-env handler k2))
	    (lambda-proc (args env2 handler k2) (k2 (apply null? args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply cons args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply car args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply cdr args)))
	    (lambda-proc (args env2 handler k2) (k2 args))
	    (lambda-proc (args env2 handler k2) (k2 (apply + args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply - args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply * args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply / args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply < args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply > args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply = args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply equal? args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply eq? args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply range args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply set-car! args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply set-cdr! args)))
	    (lambda-proc (args env2 handler k2) (import-primitive args env2 handler k2))
	    (lambda-proc (args env2 handler k2) (get-primitive args env2 handler k2))
	    (lambda-proc (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	    (lambda-proc (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	    (lambda-proc (args env2 handler k2) (k2 (apply reverse args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply append args)))
	    (lambda-proc (args env2 handler k2) (k2 (apply list->vector args)))
	    (lambda-proc (args env2 handler k2) (k2 (get-variables env2)))
	    (lambda-proc (args env2 handler k2) (k2 env2))
	    (lambda-proc (args env2 handler k2) (k2 (let ((now (current-time)))
						 (+ (time-second now)
						    (inexact (/ (time-nanosecond now)
								1000000000))))))
	    ))))

(define* get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value sym env handler
	(lambda-cont (v)
	  (cond
	    ((null? (cdr args)) (k v))
	    ((not (module? v)) (handler (format "~a is not a module" sym)))
	    (else (get-primitive (cdr args) v handler k))))))))

;; need a more reliable test for a module/environment
(define module?
  (lambda (x)
    (and (list? x) (not (null? x)) (list? (car x)))))

(define* import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  ;; temporary
	  (load-file-temp filename env handler k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler
	      (lambda-cont (binding)
		(let ((module (extend env '() '())))
		  (set-binding-value! binding module)
		  ;; temporary
		  (load-file-temp filename module handler k)))))))))

(define* call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (lambda-proc (args env2 handler k2) (k (car args)))))
      (proc (list fake-k) env handler k))))

(define get-variables
  (lambda (env)
    (map (lambda (frame) (map binding-variable frame)) env)))

(define load-stack '())

(define* load-file
  (lambda (filename env handler k)
    ;;(printf "calling load-file~%")
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k 'ok))
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
      (k 'ok)
      (read-sexp tokens handler
	(lambda-cont2 (datum tokens-left)
	  ;;(printf "read ~a~%" datum)
	  (parse datum handler
	    (lambda-cont (exp)
	      (m exp env handler
		(lambda-cont (v)
		  (load-loop tokens-left env handler k))))))))))

;; temporary
(define exception?-temp
  (lambda (x)
    (and (list? x)
	 (not (null? x))
	 (eq? (car x) 'exception))))

;; temporary version for use with non-registerized interpreter.  this
;; will be replaced by a fully registerized version of load-loop when
;; the interpreter is registerized.
(define* load-file-temp
  (lambda (filename env handler k)
    ;;(printf "calling load-file-temp~%")
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k 'ok))
      ((not (string? filename))
       (handler (format "filename is not a string: ~a" filename)))
      ((not (file-exists? filename))
       (handler (format "file does not exist: ~a" filename)))
      (else
       (set! load-stack (cons filename load-stack))
       (let ((result (scan-string (read-content filename))))
	 (if (exception?-temp result)
	     (handler (cadr result))
	     (let ((tokens result))
	       (load-loop-temp tokens env handler
		 (lambda-cont (v)
		   (set! load-stack (cdr load-stack))
		   (k v))))))))))

;; temporary version for use with non-registerized interpreter.  this
;; will be replaced by a fully registerized version of load-loop when
;; the interpreter is registerized.
(define* load-loop-temp
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
      (k 'ok)
      (let ((result (read-next-sexp tokens)))
	(if (exception?-temp result)
	  (handler (cadr result))
	  (let ((datum (car result))
		(tokens-left (cdr result)))
	    ;;(printf "read ~a~%" datum)
	    (parse datum handler
	      (lambda-cont (exp)
		(m exp env handler
		   (lambda-cont (v)
		     (load-loop-temp tokens-left env handler k)))))))))))

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

