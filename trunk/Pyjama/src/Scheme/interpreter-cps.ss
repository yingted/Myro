;; Interpreter

(load "environments-cps.ss")
(load "parser-cps.ss")

;;----------------------------------------------------------------------------

(define start
  (lambda ()
    (read-eval-print)))

(define REP-k
  (lambda (v)
    (pretty-print v)
    (read-eval-print)))

(define REP-handler
  (lambda (e)
    (REP-k `(uncaught exception: ,e))))

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
	 (parse (read-datum input)
	   (lambda (exp)
	     (m exp toplevel-env REP-handler REP-k))))))))

(define m
  (lambda (exp env handler k)
    (cases expression exp
      (lit-exp (datum) (k datum))
      (var-exp (id) (lookup-value id env handler k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler
	  (lambda (bool)
	    (if bool
	      (m then-exp env handler k)
	      (m else-exp env handler k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda (rhs-value)
	    (lookup-binding var env handler
	      (lambda (binding)
		(set-binding-value! binding rhs-value)
		(k 'ok))))))
      (define-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda (rhs-value)
	    (lookup-binding-in-first-frame var env handler
	      (lambda (binding)
		(set-binding-value! binding rhs-value)
		(k 'ok))))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler
	  (lambda (binding)
	    (set-binding-value! binding clauses)
	    (k 'ok))))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp (formals body)
	(k (closure formals body env)))
      (mu-lambda-exp (formals runt body)
	(k (mu-closure formals runt body env)))
      (try-catch-exp (body catch-var catch-exps)
	(let ((new-handler (try-catch-handler catch-var catch-exps env handler k)))
	  (m body env new-handler k)))
      (try-finally-exp (body finally-exps)
	(let ((new-handler (try-finally-handler finally-exps env handler)))
	  (m body env new-handler
	    (lambda (v)
	      ;;(printf "executing finally block~%")
	      (eval-sequence finally-exps env handler
		(lambda (v2) (k v)))))))
      (try-catch-finally-exp (body catch-var catch-exps finally-exps)
	(let ((new-handler
		(lambda (e)
		  ;;(printf "try-handler: handling ~a exception~%" e)
		  (let ((new-env (extend env (list catch-var) (list e))))
		    (let ((catch-handler (try-finally-handler finally-exps env handler)))
		      ;;(printf "executing catch block~%")
		      (eval-sequence catch-exps new-env catch-handler
			(lambda (v)
			  ;;(printf "executing finally block~%")
			  (eval-sequence finally-exps env handler
			    (lambda (v2) (k v))))))))))
	  (m body env new-handler
	     (lambda (v)
	       ;;(printf "executing finally block~%")
	       (eval-sequence finally-exps env handler
		 (lambda (v2) (k v)))))))
      (raise-exp (exp)
	(m exp env handler
	  ;; todo: pass in more info to handler (k, env)
	  (lambda (e) (handler e))))
      (app-exp (operator operands)
	(m operator env handler
	  (lambda (func)
	    (m* operands env handler
	      (lambda (vals)
		(func vals env handler k))))))
      (else (error 'm "bad abstract syntax: ~s" exp)))))

(define try-catch-handler
  (lambda (catch-var catch-exps env handler k)
    (lambda (e)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list catch-var) (list e))))
	;;(printf "executing catch block~%")
	(eval-sequence catch-exps new-env handler k)))))

(define try-finally-handler
  (lambda (finally-exps env handler)
    (lambda (e)
      ;;(printf "executing finally block~%")
      (eval-sequence finally-exps env handler
	(lambda (v)
	  ;;(printf "propagating ~a exception~%" e)
	  (handler e))))))

(define closure
  (lambda (formals body env)
    (lambda (args env2 handler k2)
      (if (= (length args) (length formals))
	(m body (extend env formals args) handler k2)
	(handler "incorrect number of arguments")))))

(define mu-closure
  (lambda (formals runt body env)
    (lambda (args env2 handler k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (m body new-env handler k2))
	(handler "not enough arguments given")))))

(define m*
  (lambda (exps env handler k)
    (if (null? exps)
      (k '())
      (m (car exps) env handler
	(lambda (v1)
	  (m* (cdr exps) env handler
	    (lambda (v2)
	      (k (cons v1 v2)))))))))

(define eval-sequence
  (lambda (exps env handler k)
    (m (car exps) env handler
       (lambda (result)
	 (if (null? (cdr exps))
	   (k result)
	   (eval-sequence (cdr exps) env handler k))))))

(define make-toplevel-env
  (lambda ()
    (make-initial-environment
      (list 'nil 'exit 'sqrt 'print 'display 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'equal? 'range 'set-car! 'set-cdr!
	    'import 'get 'call-with-current-continuation 'call/cc
	    'reverse 'append 'list->vector 'dir 'env 'current-time)
      (list '()
	    (lambda (args env2 handler k2)
	      (set! macro-env (make-macro-env))
	      (set! toplevel-env (make-toplevel-env))
	      ;; temporary
	      (set! load-stack '())
	      '(exiting the interpreter))
	    (lambda (args env2 handler k2) (k2 (apply sqrt args)))
	    (lambda (args env2 handler k2) (for-each pretty-print args) (k2 'ok))
	    (lambda (args env2 handler k2) (apply display args) (k2 'ok))
	    (lambda (args env2 handler k2) (newline) (k2 'ok))
	    (lambda (args env2 handler k2) (load-file (car args) toplevel-env handler k2))
	    (lambda (args env2 handler k2) (k2 (apply null? args)))
	    (lambda (args env2 handler k2) (k2 (apply cons args)))
	    (lambda (args env2 handler k2) (k2 (apply car args)))
	    (lambda (args env2 handler k2) (k2 (apply cdr args)))
	    (lambda (args env2 handler k2) (k2 args))
	    (lambda (args env2 handler k2) (k2 (apply + args)))
	    (lambda (args env2 handler k2) (k2 (apply - args)))
	    (lambda (args env2 handler k2) (k2 (apply * args)))
	    (lambda (args env2 handler k2) (k2 (apply / args)))
	    (lambda (args env2 handler k2) (k2 (apply < args)))
	    (lambda (args env2 handler k2) (k2 (apply > args)))
	    (lambda (args env2 handler k2) (k2 (apply = args)))
	    (lambda (args env2 handler k2) (k2 (apply equal? args)))
	    (lambda (args env2 handler k2) (k2 (apply range args)))
	    (lambda (args env2 handler k2) (k2 (apply set-car! args)))
	    (lambda (args env2 handler k2) (k2 (apply set-cdr! args)))
	    (lambda (args env2 handler k2) (import-primitive args env2 handler k2))
	    (lambda (args env2 handler k2) (get-primitive args env2 handler k2))
	    (lambda (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	    (lambda (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	    (lambda (args env2 handler k2) (k2 (apply reverse args)))
	    (lambda (args env2 handler k2) (k2 (apply append args)))
	    (lambda (args env2 handler k2) (k2 (apply list->vector args)))
	    (lambda (args env2 handler k2) (k2 (get-variables env2)))
	    (lambda (args env2 handler k2) (k2 env2))
	    (lambda (args env2 handler k2) (k2 (let ((now (current-time)))
						 (+ (time-second now)
						    (inexact (/ (time-nanosecond now)
								1000000000))))))
	    ))))

(define get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value sym env handler
	(lambda (v)
	  (if (null? (cdr args))
	    (k v)
	    (get-primitive (cdr args) v handler k)))))))

(define import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file filename env handler k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler
	      (lambda (binding)
		(let ((module (extend env '() '())))
		  (set-binding-value! binding module)
		  (load-file filename module handler k)))))))))

(define call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (lambda (args env2 handler k2) (k (car args)))))
      (proc (list fake-k) env handler k))))

(define get-variables
  (lambda (env)
    (map (lambda (frame) (map binding-variable frame)) env)))

(define load-stack '())

(define load-file
  (lambda (filename env handler k)
    ;;(printf "calling load-file~%")
    (if (member filename load-stack)
	(begin
	  (printf "skipping recursive load of ~s~%" filename)
	  (k 'ok))
	(begin
	  (set! load-stack (cons filename load-stack))
	  (let ((tokens (scan-input (read-content filename))))
	    (load-loop tokens env handler
	      (lambda (v)
		(set! load-stack (cdr load-stack))
		(k v))))))))
		
(define load-loop
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
      (k 'ok)
      (read-sexp tokens
	(lambda (datum tokens-left)
	  ;;(printf "read ~s~%" datum)
	  (parse datum
	    (lambda (exp)
	      (m exp env handler
		(lambda (v)
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
	
(define toplevel-env (make-toplevel-env))
(define macro-env (make-macro-env))

