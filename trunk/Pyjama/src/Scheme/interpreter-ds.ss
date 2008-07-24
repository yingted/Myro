;; Interpreter

(load "environments-ds.ss")
(load "parser-ds.ss")

;;----------------------------------------------------------------------------

(define apply-interpreter-cont
  (lambda (k value)
    (record-case (cdr k)
       (init () value)
       (load-loop-1 (tokens-left env handler k)
	   (load-loop tokens-left env handler k))
       (load-loop-2 (env handler tokens-left k)
           (m value env handler (make-cont 'load-loop-1 tokens-left env handler k)))
       (load-file (k)
  	   (set! load-stack (cdr load-stack))
	   (apply-cont 'interpreter k value))
       (load-file-2 (env handler k)
	   (load-loop value env handler (make-cont 'load-file k)))
       (import-prim (env filename handler k)
	   (let ((module (extend env '() '())))
	     (set-binding-value! value module)
	     (load-file filename module handler k)))
       (get-prim (args k handle sym k)
	   (cond
	    ((null? (cdr args)) (apply-cont 'interpreter k value))
	    ((not (module? value)) (handler (format "~s is not a module" sym)))
	    (else (get-primitive (cdr args) value handler k))))
       (eval-sequence (exps k env handler k)
	    (if (null? (cdr exps))
		(apply-cont 'interpreter k value)
		(eval-sequence (cdr exps) env handler k)))
       (m*-1 (k v1)
            (apply-cont 'interpreter k (cons v1 value)))
       (m*-2 (exps env handler k)
   	    (m* (cdr exps) env handler (make-cont 'm*-1 k value)))
       (try-finally-handler (handler e)
 	    ;;(printf "propagating ~a exception~%" e)
	    (handler e))

       (m-1 (func env handler k)
	    (func value env handler k))
       (m-2 (operands env handler k)
	    (m* operands env handler (make-cont 'm-1 value env handler k)))
       (m-3 (handler)
	  ;; todo: pass in more info to handler (k, env)
	  (handler value))
       (m-4 (k v)
	  (apply-cont 'interpreter k v))
       (m-5 (finally-exps env handler k)
          ;;(printf "executing finally block~%")
	  (eval-sequence finally-exps env handler (make-cont 'm-4 k value)))
       (m-6 (k v)
	  (apply-cont 'interpreter k v))
       (m-7 (finally-exps env handler k)
	  ;;(printf "executing finally block~%")
	  (eval-sequence finally-exps env handler (make-cont 'm-6 k value)))
       (m-8 (env catch-var finnaly-exps handler catch-exps k)
	  ;;(printf "try-handler: handling ~a exception~%" e)
	  (let ((new-env (extend env (list catch-var) (list value))))
	    (let ((catch-handler (try-finally-handler finally-exps env handler)))
	      ;;(printf "executing catch block~%")
	      (eval-sequence catch-exps new-env catch-handler (make-cont 'm-7 finally-exps env handler k)))))
       (m-9 (k)
	  (apply-cont 'interpreter k value))
       (m-10 (finally-exps env handler k)
	  ;;(printf "executing finally block~%")
	  (eval-sequence finally-exps env handler (make-cont 'm-9 k)))
       (m-11 (clauses k)
	  (set-binding-value! value clauses)
	  (apply-cont 'interpreter k 'ok))
       (m-12 (rhs-value k)
	  (set-binding-value! value rhs-value)
	  (apply-cont 'interpreter k 'ok))
       (m-13 (var env handler k)
	  (lookup-binding-in-first-frame var env handler (make-cont 'm-12 value k)))
       (m-14 (rhs-value k)
	  (set-binding-value! value rhs-value)
	  (apply-cont 'interpreter k 'ok))
       (m-15 (var env handler k)
	  (lookup-binding var env handler (make-cont 'm-14 value k)))
       (m-16 (then-exp env handler k)
	  (if value
	      (m then-exp env handler k)
	      (m else-exp env handler k)))
       (rep-1 ()
	  (m value toplevel-env REP-handler REP-k))
       (REP-handler ()
	  (REP-k `(uncaught exception: ,value)))
       (REP-k ()
	      (pretty-print value)
	      (read-eval-print))
       (try-catch-handler (catch-var catch-exps env handler k)
	  ;;(printf "try-handler: handling ~a exception~%" e)
	  (let ((new-env (extend env (list catch-var) (list value))))
	    ;;(printf "executing catch block~%")
	    (eval-sequence catch-exps new-env handler k)))
       (try-finally-handler (finally-exps env handler)
	  ;;(printf "executing finally block~%")
	  (eval-sequence finally-exps env handler (make-cont 'try-finally-handler handler value)))
       (else (error 'apply-parser-cont "invalid continuation: '~s'" k)))))

(define apply-interpreter-cont2
  (lambda (k datum tokens-left)
    (record-case k
       (load-loop-3 (env handler k)
	   (parse datum handler (make-cont 'load-loop-2 env handler tokens-left k)))
       (rep-2 ()
	   (parse datum REP-handler (make-cont 'rep-1)))
       (else (error 'apply-interpreter-cont2 "invalid continuation: '~s'" k)))))


(define start
  (lambda ()
    (read-eval-print)))

(define REP-k (make-cont 'REP-k))

(define REP-handler (make-cont 'REP-handler))

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
	 ;;(read-datum input REP-handler (make-cont 'interpreter 'rep-2)))))))
	 (parse (read-string input) test-handler (make-cont 'interpreter 'rep-2)))))))

(define m
  (lambda (exp env handler k)
    (cases expression exp
      (lit-exp (datum) (apply-cont 'interpreter k datum))
      (var-exp (id) (lookup-value id env handler k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler (make-cont 'm-16 then-exp env handler k)))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler (make-cont 'm-15 var env handler k))) 
      (define-exp (var rhs-exp)
	(m rhs-exp env handler (make-cont 'm-13 var env handler k)))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler (make-cont 'm-11 clauses k)))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp (formals body)
	(apply-cont 'interpreter k (closure formals body env)))
      (mu-lambda-exp (formals runt body)
	(apply-cont 'interpreter k (mu-closure formals runt body env)))
      (try-catch-exp (body catch-var catch-exps)
	(let ((new-handler (try-catch-handler catch-var catch-exps env handler k)))
	  (m body env new-handler k)))
      (try-finally-exp (body finally-exps)
	(let ((new-handler (try-finally-handler finally-exps env handler)))
	  (m body env new-handler (make-cont 'm-10 finally-exps env handler k)))) 
      (try-catch-finally-exp (body catch-var catch-exps finally-exps)
	(let ((new-handler (make-cont 'm-8 env catch-var finnaly-exps handler catch-exps k)))
	  (m body env new-handler (make-cont 'm-5 finally-exps env handler k))))
      (raise-exp (exp)
	(m exp env handler (make-cont 'm-3 handler)))
      (app-exp (operator operands)
	(m operator env handler (make-cont 'm-2 operands env handler k)))
      (else (error 'm "bad abstract syntax: ~s" exp)))))

(define try-catch-handler 
  (lambda (catch-var catch-exps env handler k)
    (make-cont 'try-catch-handler catch-var catch-exps env handler k)))

(define try-finally-handler 
  (lambda (finally-exps env handler)
    (make-cont 'try-finally-handler finally-exps env handler)))

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
      (apply-cont 'interpreter k '())
      (m (car exps) env handler (make-cont 'm*-2 exps env handler k)))))

(define eval-sequence
  (lambda (exps env handler k)
    (m (car exps) env handler (make-cont 'eval-sequence exps k env handler k))))

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
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply sqrt args)))
	    (lambda (args env2 handler k2) (for-each pretty-print args) (apply-cont 'interpreter k2 'ok))
	    (lambda (args env2 handler k2) (apply display args) (apply-cont 'interpreter k2 'ok))
	    (lambda (args env2 handler k2) (newline) (apply-cont 'interpreter k2 'ok))
	    (lambda (args env2 handler k2) (load-file (car args) toplevel-env handler k2))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply null? args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply cons args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply car args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply cdr args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 args))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply + args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply - args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply * args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply / args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply < args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply > args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply = args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply equal? args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply range args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply set-car! args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply set-cdr! args)))
	    (lambda (args env2 handler k2) (import-primitive args env2 handler k2))
	    (lambda (args env2 handler k2) (get-primitive args env2 handler k2))
	    (lambda (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	    (lambda (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply reverse args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply append args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (apply list->vector args)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (get-variables env2)))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 env2))
	    (lambda (args env2 handler k2) (apply-cont 'interpreter k2 (let ((now (current-time)))
						 (+ (time-second now)
						    (inexact (/ (time-nanosecond now)
								1000000000))))))
	    ))))

(define get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value sym env handler (make-cont 'get-prim args k handle sym k)))))

(define module?
  (lambda (x)
    (list? x)))

(define import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file filename env handler k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler (make-cont 'import-prim env filename handler k)))))))

(define my-apply ;; for invoking function in call/cc
  (lambda (proc args k)
    (proc args)
    (apply-cont 'interpreter k)))

(define call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (lambda (args env2 handler k2) 
		    (apply-cont k (car args)))))
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
	  (apply-cont 'interpreter k 'ok))
	(begin
	  (set! load-stack (cons filename load-stack))
	  (scan-input (read-content filename) handler (make-cont 'load-file-2 env handler k))))))
		
(define load-loop
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
      (apply-cont 'interpreter k 'ok)
      (read-sexp tokens handler (make-cont 'load-loop-3 env handler k)))))

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
