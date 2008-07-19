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

(define occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (k #f))
      ((pattern-variable? pattern) (k (equal? var pattern)))
      (else (occurs? var (car pattern)
	      (lambda (bool)
		(if bool
		  (k #t)
		  (occurs? var (cdr pattern) k))))))))

(define unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (unit-sub p1 p2))
	 (occurs? p1 p2
	   (lambda (bool)
	     (if bool
	       (k #f)
	       (k (unit-sub p1 p2)))))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (empty-sub)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (k #f)))))

(define unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns (car pair1) (car pair2)
      (lambda (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate (cdr pair1) s-car
	    (lambda (new-cdr1)
	      (instantiate (cdr pair2) s-car
		(lambda (new-cdr2)
		  (unify-patterns new-cdr1 new-cdr2
		    (lambda (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (compose-subs s-car s-cdr))))))))))))))

(define instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate (car pattern) s
	 (lambda (a)
	   (instantiate (cdr pattern) s
	     (lambda (b)
	       (k (cons a b)))))))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define empty-sub
  (lambda ()
    (list 'empty)))

(define extend-sub
  (lambda (old-s new-var new-pattern)
    (list 'extended new-var new-pattern old-s)))

(define unit-sub
  (lambda (var pattern)
    (list 'unit var pattern)))

(define compose-subs
  (lambda (s1 s2)
    (list 'composite s1 s2)))

(define apply-sub
  (lambda (s var k)
    (record-case s
      (empty () (k var))
      (extended (new-var new-pattern old-s)
	(if (equal? var new-var)
	  (k new-pattern)
	  (apply-sub old-s var k)))
      (unit (new-var new-pattern)
	(if (equal? var new-var)
	  (k new-pattern)
	  (k var)))
      (composite (s1 s2)
	(apply-sub s1 var
	  (lambda (pattern)
	    (instantiate pattern s2 k))))
      (else (error 'apply-sub "bad substitution: ~a" s)))))
