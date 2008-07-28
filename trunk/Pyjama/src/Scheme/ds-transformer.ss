;; todo:
;; - add apply-function calls
;; - change to lambda-proc, lambda-cont, etc.
;; - sort record fields

(load "parser-cps.ss")

(define all-source-files
  '("interpreter-cps.ss" "parser-cps.ss" "environments-cps.ss" "reader-cps.ss"
    "unifier-cps.ss" "fact.ss"
    ))

(define make-datatype-generator
  (lambda (type abbreviation params)
    (let ((make-name (string->symbol (format "make-~a" abbreviation)))
	  (apply-name (string->symbol (format "apply-~a" abbreviation)))
	  (counter 1)
	  (clauses '()))
      (if (memq make-name all-global-symbols)
	  (error 'make-datatype-generator "~a already defined at top level" make-name))
      (if (memq apply-name all-global-symbols)
	  (error 'make-datatype-generator "~a already defined at top level" apply-name))
      (lambda msg
	(record-case msg
	  (info ()
	    (let ((tag (string->symbol (format "<~a-N>" abbreviation))))
	      (printf "type: ~a~%" type)
	      (printf "(~a '~a ...)\n~a~%" make-name tag `(,apply-name ,@params))))
	  (get-type () type)
	  (get-make-name () make-name)
	  (get-clauses () clauses)
	  (reset ()
	    (set! counter 1)
	    (set! clauses '())
	    'ok)
	  (add-clause (lambda-exp fields)
	    (let* ((formals (cadr lambda-exp))
		   (bodies (cddr lambda-exp))
		   (new-lambda
		     (rename-lambda-formals
		       `(lambda ,formals ,@(map transform bodies))
		       (cdr params)))
		   (tag (string->symbol (format "<~a-~a>" abbreviation counter))))
	      (set! counter (+ counter 1))
	      (let ((new-clause `(,tag ,fields ,@(cddr new-lambda))))
		(set! clauses (cons new-clause clauses))
		tag)))
	  (print-code port
	    (let* ((output-port (if (null? port) (current-output-port) (car port)))
		   (error-string (format "bad ~a: ~~a" type))
		   (apply-function-code
		     `(define ,apply-name
			(lambda ,params
			  (record-case (cdr ,(car params))
			    ,@(reverse clauses)
			    (else (error (quote ,apply-name) ,error-string ,(car params)))))))
		   (make-function-code
		     `(define ,make-name
			(lambda args
			  (cons (quote ,type) args)))))
	      (fprintf output-port ";;----------------------------------------------------~%")
	      (fprintf output-port ";; ~a datatype~%~%" (datatype-generator 'get-type))
	      (pretty-print make-function-code output-port)
	      (newline output-port)
	      (pretty-print apply-function-code output-port)
	      (newline output-port)))
	  (else (error 'datatype-generator "bad message: ~a" msg)))))))

(define datatype-generator 'undefined)

(define transform-file
  (lambda (filename . opts)
    ;; temporary
    (if (not (member filename all-source-files))
	(error 'transform-file "\"~a\" not in all-source-files list" filename))
    (set! all-global-symbols (get-all-global-symbols))
    (set! datatype-generator (make-datatype-generator 'continuation 'cont '(k value)))
    (letrec
      ((loop (lambda (input-port output-port)
	       (let ((exp (read input-port)))
		 (if (eof-object? exp)
		     (begin
		       (datatype-generator 'print-code output-port))
		     (begin
		       (pretty-print (transform exp) output-port)
		       (newline output-port)
		       (loop input-port output-port)))))))
      (call-with-input-file filename
	(lambda (input-port)
	  (if (null? opts)
	      (begin
		(newline)
		(loop input-port (current-output-port)))
	      (let ((output-filename (car opts)))
		(call-with-output-file output-filename
		  (lambda (output-port)
		    (loop input-port output-port)
		    (printf "Output written to ~a~%" output-filename))))))))))

(define transform
  (lambda (code)
    (cond
      ((null? code) code)
      ((literal? code) code)
      ((symbol? code) code)
      (else
	(record-case code
	  (quote (datum) code)
	  (quasiquote (datum) (list 'quasiquote (transform-quasiquote datum)))
	  (if (test . conseqs)
	    `(if ,test ,@(map transform conseqs)))
	  (cond clauses
	    `(cond ,@(map transform clauses)))
	  (lambda (formals . bodies)
	    (let* ((fields (free-vars code))
		   (tag (datatype-generator 'add-clause code fields))
		   (make-name (datatype-generator 'get-make-name)))
	      `(,make-name (quote ,tag) ,@fields)))
	  (let (bindings . bodies)
	    (if (symbol? bindings)
	      ;; named let
	      (let ((name (cadr code))
		    (vars (map car (caddr code)))
		    (exps (map cadr (caddr code)))
		    (bodies (cdddr code)))
		`(let ,name ,(map list vars (map transform exps))
		   ,@(map transform bodies)))
	      ;; ordinary let
	      (let ((vars (map car bindings))
		    (exps (map cadr bindings)))
		`(let ,(map list vars (map transform exps))
		   ,@(map transform bodies)))))
	  (let* (bindings . bodies)
	    (let ((vars (map car bindings))
		  (exps (map cadr bindings)))
	      `(let* ,(map list vars (map transform exps))
		 ,@(map transform bodies))))
	  (letrec (decls . bodies)
	    `(letrec ,(transform-letrec-decls decls)
	       ,@(map transform bodies)))
	  (set! (var exp)
	    `(set! ,var ,(transform exp)))
	  (begin exps
	    `(begin ,@(map transform exps)))
	  (define (name . bodies)
	    (cond
	      ((and (symbol? name) (lambda? (car bodies)))
	       ;; (define <name> (lambda <formals> <body> ...))
	       (let ((lambda-formals (cadar bodies))
		     (lambda-bodies (cddar bodies)))
		 `(define ,name
		    (lambda ,lambda-formals
		      ,@(map transform lambda-bodies)))))
	      ((and (symbol? name) (not (lambda? (car bodies))))
		;; (define <name> <body>)
		 `(define ,name ,@(map transform bodies)))
	      ((mit-style? code)
	       ;; (define (<var> <formal> ...) <body> ...)
	       `(define ,name ,@(map transform bodies)))
	      (else (error 'transform "bad definition: ~a" code))))
	  (define-syntax (keyword rules) code)
	  (and exps
	    `(and ,@(map transform exps)))
	  (or exps
	    `(or ,@(map transform exps)))
	  (case (exp . clauses)
	    `(case ,exp ,@(transform-case-clauses clauses)))
	  (record-case (exp . clauses)
	    `(record-case ,exp ,@(transform-record-case-clauses clauses)))
	  (cases (type exp . clauses)
	    `(cases ,type ,exp ,@(transform-record-case-clauses clauses)))
	  (else (if (memq (car code) syntactic-keywords)
		  (error 'transform "don't know how to process ~a" code)
		  ;; application
		  ;; need to transform calls to apply-thing here if
		  ;;    operator is a non-globally-bound variable
		  ;;    that is recognized
		  ;;    or operator is a list of the form (make-something ...)
		  ;;    where something is recognized
		  ;; then transform to (apply-something ...)
		  (map transform code))))))))

(define transform-quasiquote
  (lambda (datum)
    (cond
      ((vector? datum) (list->vector (transform-quasiquote (vector->list datum))))
      ((not (pair? datum)) datum)
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) datum)
      ((unquote? datum) (list 'unquote (transform (cadr datum))))
      ((unquote-splicing? (car datum))
       (let* ((uqs (car datum))
	      (transformed-uqs (list 'unquote-splicing (transform (cadr uqs)))))
	 (if (null? (cdr datum))
	     (cons transformed-uqs '())
	     (cons transformed-uqs (transform-quasiquote (cdr datum))))))
      (else
	(cons (transform-quasiquote (car datum))
	      (transform-quasiquote (cdr datum)))))))

(define transform-letrec-decls
  (lambda (decls)
    (map (lambda (decl)
	   (let ((var (car decl))
		 (proc (cadr decl)))
	     (if (lambda? proc)
	       (list var `(lambda ,(cadr proc) ,@(map transform (cddr proc))))
	       (list var (transform proc)))))
      decls)))

(define transform-record-case-clauses
  (lambda (clauses)
    (map (lambda (clause)
	   (cons (car clause)
	     (cons (cadr clause)
	       (map transform (cddr clause)))))
      clauses)))

(define transform-case-clauses
  (lambda (clauses)
    (map (lambda (clause)
	   (cons (car clause) (map transform (cdr clause))))
      clauses)))

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

;;-------------------------------------------------------------------------------
;; free variables

(define free-vars
  (lambda (code)
    (free code all-global-symbols)))

(define all-free-vars
  (lambda (exps)
    (all-free exps all-global-symbols)))

(define free
  (lambda (code params)
    (cond
      ((null? code) '())
      ((literal? code) '())
      ((symbol? code) (if (memq code params) '() (list code)))
      (else
	(record-case code
	  (quote (datum) '())
	  (quasiquote (datum) (free-quasiquote datum params))
	  (if (test . conseqs) (all-free (cdr code) params))
	  (cond clauses
	    (union-all (map (lambda (clause)
			      (if (eq? (car clause) 'else)
				(all-free (cdr clause) params)
				(all-free clause params)))
			    clauses)))
	  (lambda (formals . bodies) (all-free bodies (union formals params)))
	  (let (bindings . bodies)
	    (if (symbol? bindings)
	      ;; named let
	      (let ((name (cadr code))
		    (vars (map car (caddr code)))
		    (exps (map cadr (caddr code)))
		    (bodies (cdddr code)))
		(union (all-free bodies (union name (union vars params)))
		       (all-free exps params)))
	      ;; ordinary let
	      (let ((vars (map car bindings))
		    (exps (map cadr bindings)))
		(union (all-free bodies (union vars params))
		       (all-free exps params)))))
	  (let* (bindings . bodies) (free (let*->let code) params))
	  (letrec (decls . bodies)
	    (let ((vars (map car decls))
		  (procs (map cadr decls)))
	      (union (all-free bodies (union vars params))
		     (all-free procs (union vars params)))))
	  (set! (var exp) (free exp params))
	  (begin exps (all-free exps params))
	  (define (name . bodies)
	    (cond
	      ((and (symbol? name) (lambda? (car bodies)))
	       ;; (define <name> (lambda <formals> <body> ...))
	       (let ((lambda-formals (cadar bodies))
		     (lambda-bodies (cddar bodies)))
		 (all-free lambda-bodies (union lambda-formals params))))
	      ((and (symbol? name) (not (lambda? (car bodies))))
	       ;; (define <name> <body>)
	       (free (car bodies) params))
	      ((mit-style? code)
	       ;; (define (<var> <formal> ...) <body> ...)
	       (all-free bodies (union (cdr name) params)))
	      (else (error 'free "bad definition: ~a" code))))
	  (define-syntax (keyword rules) '())
	  (and exps (all-free exps params))
	  (or exps (all-free exps params))
	  (case (exp . clauses)
	    (union (free exp params)
		   (union-all (map (lambda (clause)
				     (let ((bodies (cdr clause)))
				       (all-free bodies params)))
				   clauses))))
	  (record-case (exp . clauses)
	    (union (free exp params)
		   (union-all (map (free-in-clause params) clauses))))
	  (cases (type exp . clauses)
	    (union (free exp params)
		   (union-all (map (free-in-clause params) clauses))))
	  (else (if (memq (car code) syntactic-keywords)
		  (error 'free "don't know how to process ~a" code)
		  ;; application
		  (all-free code params))))))))

(define all-free
  (lambda (exps params)
    (union-all (map (lambda (e) (free e params)) exps))))

(define free-quasiquote
  (lambda (datum params)
    (cond
     ((vector? datum) (free-quasiquote (vector->list datum) params))
     ((not (pair? datum)) '())
     ;; doesn't handle nested quasiquotes yet
     ((quasiquote? datum) '())
     ((unquote? datum) (free (cadr datum) params))
     ((unquote-splicing? datum) (free (cadr datum) params))
     (else (union (free-quasiquote (car datum) params)
		  (free-quasiquote (cdr datum) params))))))
  
(define let*->let
  (lambda (code)
    (let ((bindings (cadr code))
	  (bodies (cddr code)))
      (letrec
	((nest
	   (lambda (bindings)
	     (if (or (null? bindings) (null? (cdr bindings)))
	       `(let ,bindings ,@bodies)
	       `(let (,(car bindings)) ,(nest (cdr bindings)))))))
	(nest bindings)))))
	   
(define free-in-clause
  (lambda (params)
    (lambda (clause)
      (if (eq? (car clause) 'else)
	(all-free (cdr clause) params)
	(let ((formals (cadr clause))
	      (bodies (cddr clause)))
	  (all-free bodies (union formals params)))))))

(define union
  (lambda (s1 s2)
    (cond
      ((symbol? s2) (union s1 (list s2)))
      ((null? s1) s2)
      ((symbol? s1) (if (memq s1 s2) s2 (cons s1 s2)))
      ((memq (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2))))))

(define union-all
  (lambda (sets)
    (if (null? sets)
      '()
      (union (car sets) (union-all (cdr sets))))))

(lambda (exp)
  (let ((foo (+ a b)))
    (lambda (x y)
      (list exp foo x))))

;; ls can be an improper list or a symbol
(define mem?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((symbol? ls) (eq? x ls))
      ((eq? (car ls) x) #t)
      (else (mem? x (cdr ls))))))

;; ls can be an improper list or a symbol
(define subst
  (lambda (old new ls)
    (cond
      ((null? ls) '())
      ((symbol? ls) (if (eq? ls old) new ls))
      ((eq? (car ls) old) (cons new (cdr ls)))
      (else (cons (car ls) (subst old new (cdr ls)))))))

;; ls1 and ls2 can be improper lists or symbols
(define lengths=?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1) (null? ls2)) #t)
      ((or (null? ls1) (null? ls2)) #f)
      ((and (symbol? ls1) (symbol? ls2)) #t)
      ((or (symbol? ls1) (symbol? ls2)) #f)
      (else (lengths=? (cdr ls1) (cdr ls2))))))

(define rename-lambda-formals
  (lambda (lambda-exp new-formals)
    (let ((formals (cadr lambda-exp)))
      ;; formals and new-formals can be improper lists
      (if (not (lengths=? formals new-formals))
	  (error 'rename-lambda-formals "mismatched lambda parameters ~a and ~a"
		 formals new-formals)
	  (letrec
	    ((loop
	       (lambda (old new exp)
		 (if (null? old)
		   exp
		   (let ((new-exp (rename-formal exp (car old) (car new))))
		     (loop (cdr old) (cdr new) new-exp))))))
	    (loop formals new-formals lambda-exp))))))

(define rename-formal
  (lambda (lambda-exp old-var new-var)
    (let ((formals (cadr lambda-exp))
	  (bodies (cddr lambda-exp)))
      (cond
	((not (mem? old-var formals))
	 (error 'rename-formal "~a is not a parameter in ~a" old-var lambda-exp))
	((eq? new-var old-var) lambda-exp)
	((mem? new-var formals)
	 (error 'rename-formal "parameter ~a already exists in ~a" new-var lambda-exp))
	((memq new-var (all-free-vars bodies))
	 (error 'rename-formal "cannot safely rename ~a to ~a in ~a"
	   old-var new-var lambda-exp))
	(else `(lambda ,(subst old-var new-var formals)
		 ,@(map (lambda (body) (rename-free-occurrences body old-var new-var))
			bodies)))))))

;; some tests:
;; ok:
;; (rename-formal '(lambda (x) (y (lambda (z) (z x)))) 'x 'foo)
;; (rename-formal '(lambda (x) (y (lambda (z x) (x z)))) 'x 'z)
;; should not work:
;; (rename-formal '(lambda (x) (y (lambda (z) (foo x)))) 'x 'foo)
;; (rename-formal '(lambda (x) (y (lambda (z foo) (z x)))) 'x 'foo)
;; (rename-formal '(lambda (x) (y (lambda (z) (z x)))) 'x 'y)
;; (rename-formal '(lambda (x) (y (lambda (z) (z x)))) 'x 'z)

;; incorrect.  example:
;; (rename-free-occurrences '(let loop ((a loop) (b a)) (loop a b c)) 'a 'foo)
;; => (let loop ((a loop) (b foo)) (loop foo b c))

(define rename-free-occurrences
  (lambda (exp old-var new-var)
    (letrec
      ((rename
	(lambda (exp)
	  (cond
	    ((null? exp) exp)
	    ((literal? exp) exp)
	    ((symbol? exp) (if (eq? exp old-var) new-var exp))
	    (else
	     (record-case exp
	       (quote (datum) exp)
	       (quasiquote (datum) (list 'quasiquote (rename-quasiquote datum)))
	       (if (test . conseqs)
		 `(if ,@(map rename (cdr exp))))
	       (cond clauses
		 `(cond ,@(map (lambda (clause)
				 (if (eq? (car clause) 'else)
				   (cons 'else (map rename (cdr clause)))
				   (map rename clause)))
			       clauses)))
	       (lambda (formals . bodies)
		 (cond
		  ((not (memq old-var (free-vars exp))) exp)
		  ((or (memq new-var (all-free-vars bodies))
		       (mem? new-var formals))
		   ;; avoid variable capture
		   (error 'rename "cannot safely rename ~a to ~a in ~a" old-var new-var exp))
		  (else `(lambda ,formals ,@(map rename bodies)))))
	       (let (bindings . bodies)
		 (if (symbol? bindings)
		   ;; named let
		     (let ((name (cadr exp))
			   (vars (map car (caddr exp)))
			   (exps (map cadr (caddr exp)))
			   (bodies (cdddr exp)))
		       `(let ,name ,(map list vars (map rename exps))
			     ,@(map rename bodies)))
		   ;; ordinary let
		   (let ((vars (map car bindings))
			 (exps (map cadr bindings)))
		     `(let ,(map list vars (map rename exps))
			,@(map rename bodies)))))
	       (let* (bindings . bodies)
		 (let ((vars (map car bindings))
		       (exps (map cadr bindings)))
		   (letrec
		     ((rename-bindings
		       (lambda (vars exps)
			 (if (null? vars)
			   '()
			   (cons `(,(car vars) ,(rename (car exps)))
				 (rename-bindings (cdr vars) (cdr exps)))))))
		     `(let* ,(rename-bindings vars exps)
			,@(map rename bodies)))))
	       (letrec (decls . bodies)
		 (let ((vars (map car decls))
		       (procs (map cadr decls)))
		   `(letrec ,(map list vars (map rename procs))
		      ,@(map rename bodies))))
	       (set! (var exp) `(set! ,var ,(rename exp)))
	       (begin exps `(begin ,@(map rename exps)))
	       (and exps `(and ,@(map rename exps)))
	       (or exps `(or ,@(map rename exps)))
	       (case (exp . clauses)
		 `(case ,(rename exp)
		    ,@(map (lambda (clause)
			     (if (eq? (car clause) 'else)
			       (cons 'else (map rename (cdr clause)))
			       (map rename clause)))
			   clauses)))
	       (record-case (exp . clauses)
		 `(record-case ,(rename exp)
		    ,@(map rename-record-case-clause clauses)))
	       (cases (type exp . clauses)
		 `(cases ,type ,(rename exp)
		    ,@(map rename-record-case-clause clauses)))
	       (else (if (memq (car exp) syntactic-keywords)
		       (error 'rename "don't know how to process ~a" exp)
		       ;; application
		       (map rename exp))))))))
       (rename-quasiquote
	 (lambda (datum)
	   (cond
	     ((vector? datum) (list->vector (rename-quasiquote (vector->list datum))))
	     ((not (pair? datum)) datum)
	     ;; doesn't handle nested quasiquotes yet
	     ((quasiquote? datum) datum)
	     ((unquote? datum) (list 'unquote (rename (cadr datum))))
	     ((unquote-splicing? (car datum))
	      (let* ((uqs (car datum))
		     (renamed-uqs (list 'unquote-splicing (rename (cadr uqs)))))
		(if (null? (cdr datum))
		    (cons renamed-uqs '())
		    (cons renamed-uqs (rename-quasiquote (cdr datum))))))
	     (else
	      (cons (rename-quasiquote (car datum))
		    (rename-quasiquote (cdr datum)))))))
       (rename-record-case-clause
	 (lambda (clause)
	   (if (eq? (car clause) 'else)
	     (cons 'else (map rename (cdr clause)))
	     (let ((tags (car clause))
		   (formals (cadr clause)))
	       `(,tags ,formals ,@(map rename (cddr clause))))))))
      (rename exp))))

;;-------------------------------------------------------------------------------

(define default-global-symbols
  (environment-symbols (scheme-environment)))

(define syntactic-keywords
  (filter (lambda (x) (not (top-level-bound? x)))
	  (environment-symbols (scheme-environment))))

(define get-global-symbols
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
	(let loop ((exp (read port)) (vars '()))
	  (cond
	    ((eof-object? exp) vars)
	    ((define? exp)
	     (if (mit-style? exp)
		 (loop (read port) (union (caadr exp) vars))
		 (loop (read port) (union (cadr exp) vars))))
	    (else (loop (read port) vars))))))))

(define get-all-global-symbols
  (lambda ()
    (union (union-all (map get-global-symbols all-source-files))
	   default-global-symbols)))

(define all-global-symbols (get-all-global-symbols))

(define show-function-signatures
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
	(let loop ((exp (read port)) (sigs '()))
	  (cond
	    ((eof-object? exp)
;;	     (reverse sigs))
	     (newline))
	    ((define? exp)
	     (cond
	       ((mit-style? exp)
		(pretty-print (cadr exp))
		(loop (read port) (cons (cadr exp) sigs)))
	       ((lambda? (caddr exp))
		(let ((sig `(,(cadr exp) ,@(cadr (caddr exp)))))
		  (pretty-print sig)
		  (loop (read port) (cons sig sigs))))
	       (else (loop (read port) sigs))))
	    (else (loop (read port) sigs))))))))
