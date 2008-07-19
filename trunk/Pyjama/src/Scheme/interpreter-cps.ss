;; Interpreter

(load "environments-cps.ss")
(load "parser-cps.ss")

;;----------------------------------------------------------------------------

(define start
  (lambda ()
    (read-eval-print)))

(define read-eval-print
  (lambda ()
    (printf "==> ")
    (let ((input (read)))
      (cond
	((eq? input 'quit)
	 (begin
	   (set! macro-env (make-macro-env))
	   (set! toplevel-env (make-toplevel-env))
	   '(exiting the interpreter)))
	((not (string? input))
	  (printf "give me a string~%")
	  (read-eval-print))
	(else
	  (parse (read-datum input)
	    (lambda (exp)
	      (m exp toplevel-env
		(lambda (v)
		  (pretty-print v)
		  (read-eval-print))))))))))

(define m
  (lambda (exp env k)
    (cases expression exp
      (lit-exp (datum) (k datum))
      (var-exp (id) (lookup-value id env k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env
	  (lambda (bool)
	    (if bool
	      (m then-exp env k)
	      (m else-exp env k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env
	  (lambda (rhs-value)
	    (lookup-binding var env
	      (lambda (binding)
		(set-binding-value! binding rhs-value)
		(k 'ok))))))
      (define-exp (var rhs-exp)
	(m rhs-exp env
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
      (begin-exp (exps) (eval-begin exps env k))
      (lambda-exp (formals body) (k (closure formals body env)))
      (mu-lambda-exp (formal body) (k (mu-closure formal body env)))
      (app-exp (operator operands)
	(m operator env
	  (lambda (func)
	    (m* operands env
	      (lambda (vals)
		(func vals k))))))
      (else (error 'm "bad abstract syntax: ~s" exp)))))

(define closure
  (lambda (formals body env)
    (lambda (args k2)
      (m body (extend env formals args) k2))))

(define mu-closure
  (lambda (formal body env)
    (lambda (args k2)
      (m body (extend env (list formal) (list args)) k2))))

(define m*
  (lambda (exps env k)
    (if (null? exps)
      (k '())
      (m (car exps) env
	(lambda (v1)
	  (m* (cdr exps) env
	    (lambda (v2)
	      (k (cons v1 v2)))))))))

(define eval-begin
  (lambda (exps env k)
    (m (car exps) env
       (lambda (result)
	 (if (null? (cdr exps))
	   (k result)
	   (eval-begin (cdr exps) env k))))))

(define make-toplevel-env
  (lambda ()
    (extend (make-empty-environment)
      (list 'nil 'sqrt 'print 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'range 'set-car! 'set-cdr!
	    'reverse 'append 'list->vector)
      (list '()
	    (lambda (args k2) (k2 (apply sqrt args)))
	    (lambda (args k2) (for-each pretty-print args) (k2 'ok))
	    (lambda (args k2) (newline) (k2 'ok))
	    (lambda (args k2) (load-file (car args) k2))
	    (lambda (args k2) (k2 (apply null? args)))
	    (lambda (args k2) (k2 (apply cons args)))
	    (lambda (args k2) (k2 (apply car args)))
	    (lambda (args k2) (k2 (apply cdr args)))
	    (lambda (args k2) (k2 args))
	    (lambda (args k2) (k2 (apply + args)))
	    (lambda (args k2) (k2 (apply - args)))
	    (lambda (args k2) (k2 (apply * args)))
	    (lambda (args k2) (k2 (apply / args)))
	    (lambda (args k2) (k2 (apply < args)))
	    (lambda (args k2) (k2 (apply > args)))
	    (lambda (args k2) (k2 (apply = args)))
	    (lambda (args k2) (k2 (apply range args)))
	    (lambda (args k2) (k2 (apply set-car! args)))
	    (lambda (args k2) (k2 (apply set-cdr! args)))
	    (lambda (args k2) (k2 (apply reverse args)))
	    (lambda (args k2) (k2 (apply append args)))
	    (lambda (args k2) (k2 (apply list->vector args)))
	    ))))

(define load-file
  (lambda (filename k)
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
		(m exp toplevel-env
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

