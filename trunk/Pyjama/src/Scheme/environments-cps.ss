;; Environments represented as data structures

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
    (lookup-binding variable env handler
      (lambda (binding)
	(k (binding-value binding))))))

(define lookup-binding
  (lambda (variable env handler k)
    (let ((binding (search-env env variable)))
      (if binding
	(k binding)
	(split-variable variable
	  (lambda (components)
	    (if components
	      (lookup-member-variable components "" env handler k)
	      (handler (format "unbound variable ~a" variable)))))))))

;; adds a new binding for var to the first frame if one doesn't exist
(define lookup-binding-in-first-frame
  (lambda (var env handler k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (k binding)
          (let ((new-binding (make-binding var 'undefined)))
            (let ((new-frame (cons new-binding frame)))
              (set-first-frame! env new-frame)
	      (k new-binding))))))))

(define lookup-module-binding
  (lambda (component env path handler k)
    (let ((binding (search-env env component)))
      (cond
	(binding (k binding))
	((string=? path "") (handler (format "unbound variable ~a" component)))
	(else (handler (format "unbound variable ~a in module ~a" component path)))))))

(define lookup-member-variable
  (lambda (components path env handler k)
    (let ((var (car components)))
      (lookup-module-binding var env path handler
	(lambda (binding)
	  (if (null? (cdr components))
	    (k binding)
	    (let ((new-path (if (string=? path "")
				(format "~a" var)
				(format "~a.~a" path var)))
		  (result (binding-value binding)))
	      (if (not (module? result))
		  (handler (format "~a is not a module" new-path))
		  (lookup-member-variable
		    (cdr components) new-path result handler k)))))))))

(define split-variable
  (lambda (variable k)
    (let ((strings (group (string->list (symbol->string variable)) #\.)))
      (if (or (member "" strings) (= (length strings) 1))
	(k #f)
	(k (map string->symbol strings))))))

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
