(define make-cont list)

(define apply-unifier-cont
  (lambda (k value)
    (record-case (cdr k)
      (init () value)
      (occurs-cont (var pattern k)
	(if value
	  (apply-cont k #t)
	  (occurs? var (cdr pattern) k)))
      (unify-patterns-cont (p1 p2 k)
	(if value
	  (apply-cont k #f)
	  (apply-cont k (make-sub 'unit p1 p2))))
      (apply-sub-cont (s2 k)
	(instantiate value s2 k))
      (instantiate-cont (a k)
	(apply-cont k (cons a value)))
      (instantiate-cont-2 (pattern s k)
	(instantiate (cdr pattern) s (make-cont 'unifier 'instantiate-cont value k)))
      (unify-pairs-1 (s-car k)
	(if (not value)
	  (apply-cont k #f)
	  (apply-cont k (make-sub 'composite s-car value))))
      (unify-pairs-2 (new-cdr1 s-car k)
	(unify-patterns new-cdr1 value (make-cont 'unifier 'unify-pairs-1 s-car k)))
      (unify-pairs-3 (pair2 s-car k)
	(instantiate (cdr pair2) s-car (make-cont 'unifier 'unify-pairs-2 value s-car k)))
      (unify-pairs-4 (pair1 pair2 k)
	(if (not value)
	  (apply-cont k #f)
	  (instantiate (cdr pair1) value
	    (make-cont 'unifier 'unify-pairs-3 pair2 value k))))
      (else (error 'apply-unifier-cont "bad continuation: ~a" k)))))

;; Unification pattern-matcher

;; continuations and substitutions represented as data structures

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
      ((constant? pattern) (apply-cont k #f))
      ((pattern-variable? pattern) (apply-cont k (equal? var pattern)))
      (else (occurs? var (car pattern) (make-cont 'unifier 'occurs-cont var pattern k))))))

(define unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (apply-cont k (make-sub 'unit p1 p2))
	 (occurs? p1 p2 (make-cont 'unifier 'unify-patterns-cont p1 p2 k))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (apply-cont k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (apply-cont k #f)))))

(define unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns (car pair1) (car pair2)
      (make-cont 'unifier 'unify-pairs-4 pair1 pair2 k))))

(define instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (apply-cont k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate (car pattern) s (make-cont 'unifier 'instantiate-cont-2 pattern s k)))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

(define apply-sub
  (lambda (s var k)
    (record-case (cdr s)
      (empty () (apply-cont k var))
      (extended (new-var new-pattern old-s)
	(if (equal? var new-var)
	  (apply-cont k new-pattern)
	  (apply-sub old-s var k)))
      (unit (new-var new-pattern)
	(if (equal? var new-var)
	  (apply-cont k new-pattern)
	  (apply-cont k var)))
      (composite (s1 s2)
	(apply-sub s1 var (make-cont 'unifier 'apply-sub-cont s2 k)))
      (else (error 'apply-sub "bad substitution: ~a" s)))))
