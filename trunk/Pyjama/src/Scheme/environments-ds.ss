;; Environments represented as data structures

;; continuations represented as data structures

;; bindings

(define make-binding
  (lambda (variable value)
    (cons variable value)))

(define binding-variable
  (lambda (binding)
    (car binding)))

(define binding-value
  (lambda (binding)
    (cdr binding)))

(define set-binding-value!
  (lambda (binding value)
    (set-cdr! binding value)))

;; frames

(define make-frame
  (lambda (variables values)
    (map make-binding variables values)))

(define first-binding
  (lambda (frame)
    (car frame)))

(define rest-of-bindings
  (lambda (frame)
    (cdr frame)))

(define empty-frame?
  (lambda (frame)
    (null? frame)))

(define search-frame
  (lambda (frame variable)
    (cond
      ((empty-frame? frame) #f)
      ((eq? (binding-variable (first-binding frame)) variable)
       (first-binding frame))
      (else (search-frame (rest-of-bindings frame) variable)))))

;; environments

(define make-empty-environment
  (lambda ()
    '(())))

(define make-initial-environment
  (lambda (vars vals)
    (list (make-frame vars vals))))

(define first-frame
  (lambda (env)
    (car env)))

(define rest-of-frames
  (lambda (env)
    (cdr env)))

(define set-first-frame!
  (lambda (env new-frame)
    (set-car! env new-frame)))

(define extend
  (lambda (env variables values)
    (cons (make-frame variables values) env)))

;; variable lookup

(define search-env
  (lambda (env variable)
    (if (null? env)
      #f
      (let ((binding (search-frame (first-frame env) variable)))
        (if binding
          binding
          (search-env (rest-of-frames env) variable))))))

(define lookup-value
  (lambda (variable env handler k)
    (lookup-binding variable env handler (make-cont 'parser 'lookup-cont k))))

(define lookup-binding
  (lambda (variable env handler k)
    (let ((binding (search-env env variable)))
      (if binding
	(apply-cont k binding)
	(split-variable variable
	  (make-cont 'interpreter 'split-var-cont variable env handler k))))))

;; adds a new binding for var to the first frame if one doesn't exist
(define lookup-binding-in-first-frame
  (lambda (var env handler k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (apply-cont k binding)
          (let ((new-binding (make-binding var 'undefined)))
            (let ((new-frame (cons new-binding frame)))
              (set-first-frame! env new-frame)
	      (apply-cont k new-binding))))))))

(define lookup-variable-components
  (lambda (components path env handler k)
    (let ((var (car components)))
      (lookup-module-binding var env path handler
	(make-cont 'interpreter 'lookup-module-var-cont
	  components var path handler k)))))

(define lookup-module-binding
  (lambda (var env path handler k)
    (let ((binding (search-env env var)))
      (cond
	(binding (apply-cont k binding))
	((string=? path "")
	 (interp-apply-handler handler (format "unbound variable ~a" var)))
	(else (interp-apply-handler handler
		(format "unbound variable ~a in module ~a" var path)))))))

(define split-variable
  (lambda (variable k)
    (let ((strings (group (string->list (symbol->string variable)) #\.)))
      (if (or (member "" strings) (= (length strings) 1))
	(apply-cont k #f)
	(apply-cont k (map string->symbol strings))))))

(define group
  (lambda (chars delimiter)
    (letrec
      ((position
	(lambda (chars)
	  (if (char=? (car chars) delimiter)
	    0
	    (+ 1 (position (cdr chars))))))
       (group
	 (lambda (chars)
	   (cond
	     ((null? chars) '())
	     ((not (member delimiter chars)) (list (apply string chars)))
	     (else (let ((n (position chars)))
		     (cons (apply string (list-head chars n))
			   (group (cdr (list-tail chars n))))))))))
      (group chars))))
