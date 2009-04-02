;;; LET-OPTIONALS macros
;;; Copyright (c) 2001 by Olin Shivers.
;;; Copyright (c) 2006 by Aaron Hsu.

;;; This is a Chez Scheme version of the LET-OPTIONALS macros.
;;;
;;; Very little has been changed from the original except for a few minor 
;;; adaptations to Chez. - Aaron Hsu

(module let-opt
  (;let-optionals
   (let-optionals* %let-optionals*)
   :optional)
  (import scheme)

;;; (LET-OPTIONALS  args ((var1 default1 [arg-test supplied?]) ...) body1 ...)
;;; (LET-OPTIONALS* args ((var1 default1 [arg-test supplied?]) ...) body1 ...)

;(define-syntax let-optionals expand-let-optionals)
;(define-syntax let-optionals* expand-let-optionals*)

;;; (:optional rest-arg default-exp [test-pred])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;; 
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.
;;;
;;; If there is an TEST-PRED form, it is a predicate that is used to test
;;; a non-default value. If the predicate returns false, an error is raised.

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg)) (car maybe-arg)
	       (error ':optional "too many optional arguments: ~s" maybe-arg))
	   default-exp)))

    ((:optional rest default-exp arg-test)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg))
	       (let ((val (car maybe-arg)))
		 (if (arg-test val) val
		     (error ':optional "Optional argument failed test: ~s ~s"
			    'arg-test val)))
	       (error ':optional "too many optional arguments: ~s" maybe-arg))
	   default-exp)))))

;;; Here is a simpler but less-efficient version of LET-OPTIONALS*.
;;; It redundantly performs end-of-list checks for every optional var,
;;; even after the list runs out.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* arg (opt-clause ...) body ...)
     (let ((rest arg))
       (%let-optionals* rest (opt-clause ...) body ...)))))

;;; The arg-list expression *must* be a variable.
;;; (Or must be side-effect-free, in any event.)

(define-syntax %let-optionals*
  (syntax-rules ()
    ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
     (call-with-values (lambda () (xparser arg))
       (lambda (rest var ...)
         (%let-optionals* rest (opt-clause ...) body ...))))
    
    ((%let-optionals* arg ((var default) opt-clause ...) body ...)
     (call-with-values (lambda () (if (null? arg) (values default '())
				      (values (car arg) (cdr arg))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default '())
			     (let ((var (car arg)))
			       (if test (values var (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default #f '())
			     (let ((var (car arg)))
			       (if test (values var #t (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var supplied? rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg (rest) body ...)
     (let ((rest arg)) body ...))

    ((%let-optionals* arg () body ...)
     (if (null? arg) (begin body ...)
	 (error "Too many arguments in let-opt" arg)))))

)
