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
      (var-exp (id) (lookup-value id env k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler
	  (lambda (bool)
	    (if bool
	      (m then-exp env handler k)
	      (m else-exp env handler k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda (rhs-value)
	    (lookup-binding var env
	      (lambda (binding)
		(set-binding-value! binding rhs-value)
		(k 'ok))))))
      (define-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda (rhs-value)
	    (lookup-binding-in-first-frame var env
	      (lambda (binding)
		(set-binding-value! binding rhs-value)
		(k 'ok))))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env
	  (lambda (binding)
	    (set-binding-value! binding clauses)
	    (k 'ok))))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp (formals body) (k (closure formals body env)))
      (mu-lambda-exp (formal body) (k (mu-closure formal body env)))
      (try-exp (body catch-var catch-exps finally-exps)
	(let ((new-handler
		(lambda (e)
		  ;;(printf "try-handler: handling ~a exception~%" e)
		  (let ((new-env (extend env (list catch-var) (list e))))
		    (let ((catch-handler
			   (lambda (e)
			     ;;(printf "catch-handler: handling ~a exception~%" e)
			     (if (null? finally-exps)
			       (begin
				 ;;(printf "propagating ~a exception~%" e)
				 (handler e))
			       (begin
				 ;;(printf "executing finally block~%")
				 (eval-sequence finally-exps env handler
				   (lambda (v)
				     ;;(printf "propagating ~a exception~%" e)
				     (handler e))))))))
		      ;;(printf "executing catch block~%")
		      (eval-sequence catch-exps new-env catch-handler
			(lambda (v)
			  (if (null? finally-exps)
			    (k v)
			    (begin
			      ;;(printf "executing finally block~%")
			      (eval-sequence finally-exps env handler
				(lambda (v2) (k v))))))))))))
	  (if (null? finally-exps)
	    (m body env new-handler k)
	    (m body env new-handler
	      (lambda (v)
		;;(printf "executing finally block~%")
		(eval-sequence finally-exps env handler
		  (lambda (v2) (k v))))))))
      (try-finally-exp (body finally-exps)
	(let ((new-handler
		(lambda (e)
		  ;;(printf "executing finally block~%")
		  (eval-sequence finally-exps env handler
		    (lambda (v)
		      ;;(printf "propagating ~a exception~%" e)
		      (handler e))))))
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

(define closure
  (lambda (formals body env)
    (lambda (args env2 handler k2)
      (m body (extend env formals args) handler k2))))

(define mu-closure
  (lambda (formal body env)
    (lambda (args env2 handler k2)
      (m body (extend env (list formal) (list args)) handler k2))))

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
    (extend (make-empty-environment)
      (list 'nil 'exit 'sqrt 'print 'display 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'equal? 'range 'set-car! 'set-cdr!
	    'reverse 'append 'list->vector 'dir 'current-time)
      (list '()
	    (lambda (args env2 handler k2)
	      (set! macro-env (make-macro-env))
	      (set! toplevel-env (make-toplevel-env))
	      '(exiting the interpreter))
	    (lambda (args env2 handler k2) (k2 (apply sqrt args)))
	    (lambda (args env2 handler k2) (for-each pretty-print args) (k2 'ok))
	    (lambda (args env2 handler k2) (k2 (display args)))
	    (lambda (args env2 handler k2) (newline) (k2 'ok))
	    (lambda (args env2 handler k2) (load-file (car args) handler k2))
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
	    (lambda (args env2 handler k2) (k2 (apply reverse args)))
	    (lambda (args env2 handler k2) (k2 (apply append args)))
	    (lambda (args env2 handler k2) (k2 (apply list->vector args)))
	    (lambda (args env2 handler k2) (k2 (get-variables env2)))
	    (lambda (args env2 handler k2) (k2 (let ((now (current-time)))
						 (+ (time-second now)
						    (inexact (/ (time-nanosecond now)
							       1000000000))))))
	    ))))

(define get-variables
  (lambda (env)
    (letrec
      ((get-variables-from-frame
	 (lambda (frame)
	   (if (empty-frame? frame)
	     '()
	     (cons (binding-variable (first-binding frame))
		   (get-variables-from-frame (rest-of-bindings frame)))))))
      (if (empty-environment? env)
	'()
	(cons (get-variables-from-frame (first-frame env))
	      (get-variables (rest-of-frames env)))))))

(define load-file
  (lambda (filename handler k)
    (set! tokens_reg (scan-input (read-content filename)))
    (let loop ()
      (if (token-type? (first tokens_reg) 'end-marker)
	(k 'ok)
	(begin
	  (set! k_reg (list 'load-cont))
	  (set! pc read-sexp)
	  (let ((datum (run)))
	    (parse datum
	      (lambda (exp)
		(m exp toplevel-env handler
		  (lambda (v) (loop)))))))))))

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

