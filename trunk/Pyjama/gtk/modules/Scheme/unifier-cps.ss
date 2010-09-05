(load "transformer-macros.ss")

;; Unification pattern-matcher

(define pattern?
  (lambda (x)
    (or (null? x)
	(number? x)
	(boolean? x)
	(symbol? x)
	(and (pair? x)
	     (pattern? (car x))
	     (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
	 (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x))
	 (not (pair? x)))))

(define* occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (k #f))
      ((pattern-variable? pattern) (k (equal? var pattern)))
      (else (occurs? var (car pattern)
	      (lambda-cont (bool)
		(if bool
		  (k #t)
		  (occurs? var (cdr pattern) k))))))))

(define* unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (make-sub 'unit p1 p2))
	 (occurs? p1 p2
	   (lambda-cont (bool)
	     (if bool
	       (k #f)
	       (k (make-sub 'unit p1 p2)))))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (k #f)))))

(define* unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns (car pair1) (car pair2)
      (lambda-cont (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate (cdr pair1) s-car
	    (lambda-cont (new-cdr1)
	      (instantiate (cdr pair2) s-car
		(lambda-cont (new-cdr2)
		  (unify-patterns new-cdr1 new-cdr2
		    (lambda-cont (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (make-sub 'composite s-car s-cdr))))))))))))))

(define* instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate (car pattern) s
	 (lambda-cont (a)
	   (instantiate (cdr pattern) s
	     (lambda-cont (b)
	       (k (cons a b)))))))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

;;(define extend-sub
;;  (lambda (old-s new-var new-pattern)
;;    (list 'extended new-var new-pattern old-s)))

(define* apply-sub
  (lambda (s var k)
    (record-case (cdr s)
      (empty () (k var))
;;      (extended (new-var new-pattern old-s)
;;	(if (equal? var new-var)
;;	  (k new-pattern)
;;	  (apply-sub old-s var k)))
      (unit (new-var new-pattern)
	(if (equal? var new-var)
	  (k new-pattern)
	  (k var)))
      (composite (s1 s2)
	(apply-sub s1 var
	  (lambda-cont (pattern)
	    (instantiate pattern s2 k))))
      (else (error 'apply-sub "bad substitution: ~a" s)))))
