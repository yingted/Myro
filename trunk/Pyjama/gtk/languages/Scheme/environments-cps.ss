(load "transformer-macros.ss")

;; Environments represented as data structures

;; bindings

(define make-binding
  (lambda (variable value)
    (list variable "" value)))

(define binding-variable
  (lambda (binding)
    (car binding)))

(define binding-docstring
  (lambda (binding)
    (cadr binding)))

(define binding-value
  (lambda (binding)
    (caddr binding)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-car! (cdr binding) docstring)))

(define set-binding-value!
  (lambda (binding value)
    (set-car! (cddr binding) value)))

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

;; <environment> = (environment . (<frame> ...))

(define environment?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda ()
    (cons 'environment '(()))))

(define make-initial-environment
  (lambda (vars vals)
    (cons 'environment (list (make-frame vars vals)))))

(define first-frame
  (lambda (env)
    (cadr env)))

(define frames
  (lambda (env)
    (cdr env)))

(define set-first-frame!
  (lambda (env new-frame)
    (set-car! (cdr env) new-frame)))

(define extend
  (lambda (env variables values)
    (cons 'environment (cons (make-frame variables values) (cdr env)))))

;; variable lookup

(define search-env
  (lambda (env variable)
    (search-frames (cdr env) variable)))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
      #f
      (let ((binding (search-frame (car frames) variable)))
        (if binding
          binding
          (search-frames (cdr frames) variable))))))

(define* lookup-value
  (lambda (variable env handler k)
    (lookup-binding variable env handler
      (lambda-cont (binding)
	(k (binding-value binding))))))

(define* lookup-binding
  (lambda (variable env handler k)
    (let ((binding (search-env env variable)))
      (if binding
	(k binding)
	(split-variable variable
	  (lambda-cont (components)
            (if (dlr-env-contains variable)
                (k (dlr-env-lookup variable))
                (if components
		    (lookup-variable-components components "" env handler k)
                    (handler (format "unbound variable ~a" variable))))))))))

(define dlr-env-contains
  (lambda (variable)
    #t))

(define dlr-env-lookup
  (lambda (variable)
    (binding 42)))

;; adds a new binding for var to the first frame if one doesn't exist
(define* lookup-binding-in-first-frame
  (lambda (var env handler k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (k binding)
          (let ((new-binding (make-binding var 'undefined)))
            (let ((new-frame (cons new-binding frame)))
              (set-first-frame! env new-frame)
	      (k new-binding))))))))

(define* lookup-variable-components
  ;; math.x.y.z
  ;; components: '(test x y z) "" ...
  ;; components: '(x y z) "test" ...
  ;; components: '(y z) "test.x" ...
  ;; components: '(z) "test.x.z" ...
  (lambda (components path env handler k)
    ;;(printf "components: ~s path: ~s\n" components path)
    (let ((var (car components)))
      (lookup-module-binding var env path handler
	(lambda-cont (binding)
	  (if (null? (cdr components))
	    (k binding)
	    (let ((result (binding-value binding))
		  (new-path (if (string=? path "")
			      (format "~a" var)
			      (format "~a.~a" path var))))
	      (if (not (environment? result))
		  (handler (format "~a is not a module" new-path))
		  (lookup-variable-components
		    (cdr components) new-path result handler k)))))))))

(define* lookup-module-binding
  (lambda (var env path handler k)
    (let ((binding (search-env env var)))
      (cond
	(binding (k binding))
	((string=? path "") (handler (format "unbound variable ~a" var)))
	(else (handler (format "unbound variable ~a in module ~a" var path)))))))

(define* split-variable
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
