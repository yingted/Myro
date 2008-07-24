(define make-cont list)

(define apply-uni-cont
  (lambda (k value)
    (record-case k
      (init () value)
      (occurs-cont (var pattern k)
	(if value
	  (apply-uni-cont k #t)
	  (occurs? var (cdr pattern) k)))
      (unify-patterns-cont (p1 p2 k)
	(if value
	    (apply-uni-cont k #f)
	    (apply-uni-cont k (unit-sub p1 p2))))
      (apply-sub-cont (s2 k)
	(instantiate value s2 k))
      (instantiate-cont (a k)
	(apply-uni-cont k (cons a value)))
      (instantiate-cont-2 (pattern s k)
	(instantiate (cdr pattern) s (make-cont 'instantiate-cont value k)))
      (unify-pairs-1 (s-car k)
	(if (not value)
	    (apply-uni-cont k #f)
	    (apply-uni-cont k (compose-subs s-car value))))
      (unify-pairs-2 (new-cdr1 s-car k)
	(unify-patterns new-cdr1 value (make-cont 'unify-pairs-1 s-car k)))
      (unify-pairs-3 (pair2 s-car k)
	(instantiate (cdr pair2) s-car (make-cont 'unify-pairs-2 value s-car k)))
      (unify-pairs-4 (pair1 pair2 k)
	(if (not value)
	    (apply-uni-cont k #f)
	    (instantiate (cdr pair1) value (make-cont 'unify-pairs-3 pair2 value k))))
      (else (error 'apply-uni-cont "invalid continuation: ~s" k)))))

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
      ((constant? pattern) (apply-uni-cont k #f))
      ((pattern-variable? pattern) (apply-uni-cont k (equal? var pattern)))
      (else (occurs? var (car pattern) (make-cont 'occurs-cont var pattern k))))))

(define unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (apply-uni-cont k (unit-sub p1 p2))
	 (occurs? p1 p2 (make-cont 'unify-patterns-cont p1 p2 k))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (apply-uni-cont k (empty-sub)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (apply-uni-cont k #f)))))

(define unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns (car pair1) (car pair2) (make-cont 'unify-pairs-4 pair1 pair2 k))))

(define instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (apply-uni-cont k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate (car pattern) s (make-cont 'instantiate-cont-2 pattern s k)))
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
      (empty () (apply-uni-cont k var))
      (extended (new-var new-pattern old-s)
	(if (equal? var new-var)
	  (apply-uni-cont k new-pattern)
	  (apply-sub old-s var k)))
      (unit (new-var new-pattern)
	(if (equal? var new-var)
	  (apply-uni-cont k new-pattern)
	  (apply-uni-cont k var)))
      (composite (s1 s2)
	(apply-sub s1 var (make-cont 'apply-sub-cont s2 k)))
      (else (error 'apply-sub "bad substitution: ~a" s)))))
