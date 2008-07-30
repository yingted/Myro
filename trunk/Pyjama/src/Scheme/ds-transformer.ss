;; todo:
;; - add apply-function calls

(load "parser-cps.ss")

(define all-source-files
  '("interpreter-cps.ss" "parser-cps.ss" "environments-cps.ss" "reader-cps.ss"
    "unifier-cps.ss" "fact.ss"
    ))

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

(define make-datatype-generator
  (lambda (type abbreviation params field-order)
    (let ((make-name (string->symbol (format "make-~a" abbreviation)))
	  (apply-name (string->symbol (format "apply-~a" abbreviation)))
	  (counter 0)
	  (clauses '()))
      (let ((globals (get-all-global-symbols)))
	(if (memq make-name globals)
	    (error 'source "~a is already defined at top level" make-name))
	(if (memq apply-name globals)
	    (error 'source "~a is already defined at top level" apply-name)))
      (lambda msg
	(record-case msg
	  (info ()
	    (let ((tag (string->symbol (format "<~a-N>" abbreviation))))
	      (printf "type: ~a~%" type)
	      (printf "(~a '~a ...)\n~a~%" make-name tag `(,apply-name ,@params))))
	  (get-make-name () make-name)
	  (get-apply-name () apply-name)
	  (get-field-order () field-order)
	  (reset ()
	    (set! counter 0)
	    (set! clauses '())
	    'ok)
	  (add-clause (formals bodies fields)
	    (let* ((new-lambda
		     (rename-lambda-formals
		       type (cdr params) `(lambda ,formals ,@(map transform bodies))))
		   (new-code (cddr new-lambda))
		   (identical-clause (find-clause fields new-code clauses)))
	      (if identical-clause
		(car identical-clause)
		(begin
		  (set! counter (+ counter 1))
		  (let* ((new-tag (string->symbol (format "<~a-~a>" abbreviation counter)))
			 (new-clause `(,new-tag ,fields ,@new-code)))
		    (set! clauses (cons new-clause clauses))
		    new-tag)))))
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
	      (fprintf output-port ";; ~a datatype~%~%" type)
	      (pretty-print make-function-code output-port)
	      (newline output-port)
	      (pretty-print apply-function-code output-port)
	      (newline output-port)))
	  (else (error 'datatype-generator "bad message: ~a" msg)))))))

(define find-clause
  (lambda (fields code clauses)
    (cond
      ((null? clauses) #f)
      ((and (equal? (cadar clauses) fields) (equal? (cddar clauses) code))
       (car clauses))
      (else (find-clause fields code (cdr clauses))))))

(define transform-file
  (lambda (sourcefile . opts)
    ;; temporary
    (if (not (member sourcefile all-source-files))
	(error #f "\"~a\" is not in all-source-files list" sourcefile))
    (set! all-global-symbols (get-all-global-symbols))
    (datatype-generator 'reset)
    (letrec
      ((load? (lambda (x) (and (list? x) (not (null? x)) (eq? (car x) 'load))))
       (loop (lambda (input-port output-port)
	       (let ((exp (read input-port)))
		 (cond
		   ((eof-object? exp) (datatype-generator 'print-code output-port))
		   ;; ignore calls to load at top level
		   ((load? exp) (loop input-port output-port))
		   (else (pretty-print (transform exp) output-port)
			 (newline output-port)
			 (loop input-port output-port)))))))
      (call-with-input-file sourcefile
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

(define datatype-lambda?
  (lambda (code)
    (and (list? code)
	 (not (null? code))
	 (symbol? (car code))
	 (memq (car code) '(lambda-cont lambda-cont2 lambda-proc lambda-handler)))))

(define datatype-application?
  (lambda (code)
    (and (symbol? (car code))
;;	 (not (memq (car code) all-global-symbols))
	 (memq (car code) '(k k2 handler proc)))))

(define transform
  (lambda (code)
    (cond
      ((null? code) code)
      ((literal? code) code)
      ((symbol? code) code)
      ((datatype-lambda? code)
       (let* ((formals (cadr code))
	      (bodies (cddr code))
	      (field-order (datatype-generator 'get-field-order))
	      (fields (sort-fields (free-vars code) field-order))
	      (tag (datatype-generator 'add-clause formals bodies fields))
	      (make-name (datatype-generator 'get-make-name)))
	 `(,make-name (quote ,tag) ,@fields)))
      ((datatype-application? code)
;       (printf "datatype application: ~a~%" code)
       (transform (cons (datatype-generator 'get-apply-name) code)))
      (else
	(record-case code
	  (quote (datum) code)
	  (quasiquote (datum)
	    (list 'quasiquote (transform-quasiquote datum)))
	  (if (test . conseqs)
	    `(if ,(transform test) ,@(map transform conseqs)))
	  (cond clauses
	    `(cond ,@(map transform-cond-clause clauses)))
	  (lambda (formals . bodies)
	    `(lambda ,formals ,@(map transform bodies)))
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
	  (set! (var rhs-exp)
	    `(set! ,var ,(transform rhs-exp)))
	  (begin exps
	    `(begin ,@(map transform exps)))
	  (define (name . bodies)
	    (if (mit-style? code)
	      `(define ,name ,@(map transform bodies))
	      `(define ,name ,(transform (car bodies)))))
	  (define-syntax (keyword rules) code)
	  (and exps
	    `(and ,@(map transform exps)))
	  (or exps
	    `(or ,@(map transform exps)))
	  (case (exp . clauses)
	    `(case ,(transform exp) ,@(transform-case-clauses clauses)))
	  (record-case (exp . clauses)
	    `(record-case ,(transform exp) ,@(transform-rc-clauses clauses)))
	  (cases (type exp . clauses)
	    `(cases ,type ,(transform exp) ,@(transform-rc-clauses clauses)))
	  (else (if (memq (car code) syntactic-keywords)
		  (error 'transform "don't know how to process ~a" code)
		  (map transform code))))))))

(define transform-cond-clause
  (lambda (clause)
    (if (eq? (car clause) 'else)
      `(else ,@(map transform (cdr clause)))
      `(,(transform (car clause)) ,@(map transform (cdr clause))))))

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

(define transform-rc-clauses
  (lambda (clauses)
    (map (lambda (clause)
	   (if (eq? (car clause) 'else)
	     `(else ,@(map transform (cdr clause)))
	     (cons (car clause) (cons (cadr clause) (map transform (cddr clause))))))
	 clauses)))

(define transform-case-clauses
  (lambda (clauses)
    (map (lambda (clause) (cons (car clause) (map transform (cdr clause))))
	 clauses)))

(define sort-fields
  (lambda (fields field-order)
    (letrec
	((pos (lambda (x ls)
		(if (eq? (car ls) x)
		  0
		  (+ 1 (pos x (cdr ls)))))))
      (sort (lambda (f1 f2)
	      (let ((f1-mem (memq f1 field-order))
		    (f2-mem (memq f2 field-order)))
		(cond
		  ((and f1-mem f2-mem) (< (pos f1 field-order) (pos f2 field-order)))
		  ((and (not f1-mem) f2-mem) #t)
		  ((and f1-mem (not f2-mem)) #f)
		  (else (< (pos f1 fields) (pos f2 fields))))))
	    fields))))

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
      ((datatype-lambda? code) (all-free (cddr code) (union (cadr code) params)))
      ((symbol? code) (if (memq code params) '() (list code)))
      (else
	(record-case code
	  (quote (datum) '())
	  (quasiquote (datum) (free-in-quasiquote datum params))
	  (if (test . conseqs) (all-free (cdr code) params))
	  (cond clauses
	    (union-all (map (lambda (clause)
			      (if (eq? (car clause) 'else)
				(all-free (cdr clause) params)
				(all-free clause params)))
			    clauses)))
	  (lambda (formals . bodies) (all-free bodies (union formals params)))
	  (let (bindings . bodies) (free (let-transformer code) params))
	  (let* (bindings . bodies) (free (let*-transformer code) params))
	  (letrec (decls . bodies) (free (letrec-transformer code) params))
	  (set! (var rhs-exp) (free rhs-exp params))
	  (begin exps (all-free exps params))
	  (define (name . bodies)
	    (if (mit-style? code)
	      (all-free bodies (union name params))
	      (free (car bodies) (union name params))))
	  (define-syntax (keyword rules) '())
	  (and exps (all-free exps params))
	  (or exps (all-free exps params))
	  (case (exp . clauses)
	    (union (free exp params)
		   (union-all (map (lambda (clause) (all-free (cdr clause) params))
				   clauses))))
	  (record-case (exp . clauses)
	    (union (free exp params)
		   (union-all (map (free-in-record-case-clause params) clauses))))
	  (cases (type exp . clauses)
	    (union (free exp params)
		   (union-all (map (free-in-record-case-clause params) clauses))))
	  (else (if (memq (car code) syntactic-keywords)
		  (error 'free "don't know how to process ~a" code)
		  ;; application
		  (all-free code params))))))))

(define all-free
  (lambda (exps params)
    (union-all (map (lambda (e) (free e params)) exps))))

(define free-in-quasiquote
  (lambda (datum params)
    (cond
     ((vector? datum) (free-in-quasiquote (vector->list datum) params))
     ((not (pair? datum)) '())
     ;; doesn't handle nested quasiquotes yet
     ((quasiquote? datum) '())
     ((unquote? datum) (free (cadr datum) params))
     ((unquote-splicing? datum) (free (cadr datum) params))
     (else (union (free-in-quasiquote (car datum) params)
		  (free-in-quasiquote (cdr datum) params))))))
  
(define free-in-record-case-clause
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
  (lambda (type new-formals lambda-exp)
    (let ((formals (cadr lambda-exp)))
      ;; formals and new-formals can be improper lists
      (if (not (lengths=? formals new-formals))
	  (error 'source "lambda parameters do not match ~a parameters ~a:\n~a\n"
		 type new-formals lambda-exp)
	  (letrec
	    ((loop
	       (lambda (old new exp)
		 (if (null? old)
		   exp
		   (let ((new-exp (rename-formal (car old) (car new) exp)))
		     (loop (cdr old) (cdr new) new-exp))))))
	    (loop formals new-formals lambda-exp))))))

(define rename-formal
  (lambda (old-var new-var lambda-exp)
    (let ((formals (cadr lambda-exp))
	  (bodies (cddr lambda-exp)))
      (cond
	((not (mem? old-var formals))
	 (error 'rename-formal "~a is not a parameter in ~a" old-var lambda-exp))
	((eq? new-var old-var) lambda-exp)
	((mem? new-var formals)
	 (error 'rename-formal "parameter ~a already exists in ~a" new-var lambda-exp))
	((memq new-var (all-free bodies '()))
	 (error 'rename-formal "cannot safely rename ~a to ~a in ~a"
	   old-var new-var lambda-exp))
	((ormap (contains-unsafe-lambda? old-var new-var) bodies)
	 (error 'rename-formal "cannot safely rename ~a to ~a in ~a"
	   old-var new-var lambda-exp))
	(else `(lambda ,(subst old-var new-var formals)
		 ,@(map (rename-free-occurrences old-var new-var)
			bodies)))))))

;; some tests:
;; ok:
;; (rename-formal 'x 'foo '(lambda (x) (y (lambda (z) (z x)))))
;; (rename-formal 'x 'z '(lambda (x) (y (lambda (z x) (x z)))))
;; (rename-formal 'a 'foo '(lambda (a) (let loop ((a loop) (b a)) (loop a b c))))
;; (rename-formal 'list 'cdr '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; should not work:
;; (rename-formal 'x 'foo '(lambda (x) (y (lambda (z) (foo x)))))
;; (rename-formal 'x 'foo '(lambda (x) (y (lambda (z foo) (z x)))))
;; (rename-formal 'x 'y '(lambda (x) (y (lambda (z) (z x)))))
;; (rename-formal 'x 'z '(lambda (x) (y (lambda (z) (z x)))))
;; (rename-formal 'a 'loop '(lambda (a) (let loop ((a loop) (b a)) (loop a b c))))
;; (rename-formal 'list 'c '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; (rename-formal 'list 'y '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))

(define rename-free-occurrences
  (lambda (old-var new-var)
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
	       (quasiquote (datum)
		 (list 'quasiquote (rename-in-quasiquote datum)))
	       (if (test . conseqs)
		 `(if ,@(map rename (cdr exp))))
	       (cond clauses
		 `(cond ,@(map (lambda (clause)
				 (if (eq? (car clause) 'else)
				   (cons 'else (map rename (cdr clause)))
				   (map rename clause)))
			       clauses)))
	       (lambda (formals . bodies)
		 (if (mem? old-var formals)
		   exp
		   `(lambda ,formals ,@(map rename bodies))))
	       (let (bindings . bodies)
		 (if (symbol? bindings)
		   ;; named let
		   (let ((name (cadr exp))
			 (vars (map car (caddr exp)))
			 (exps (map cadr (caddr exp)))
			 (bodies (cdddr exp)))
		     `(let ,name ,(map list vars (map rename exps))
			,@(if (or (memq old-var vars) (eq? old-var name))
			    bodies
			    (map rename bodies))))
		   ;; ordinary let
		   (let ((vars (map car bindings))
			 (exps (map cadr bindings)))
		     `(let ,(map list vars (map rename exps))
			,@(if (memq old-var vars)
			    bodies
			    (map rename bodies))))))
	       (let* (bindings . bodies)
		 (let ((vars (map car bindings)))
		   (letrec
		     ((rename-binding-exps
		       (lambda (bindings)
			 (if (null? bindings)
			   '()
			   (let* ((var (caar bindings))
				  (exp (cadar bindings))
				  (new-binding (list var (rename exp))))
			     (if (eq? var old-var)
			       (cons new-binding (cdr bindings))
			       (cons new-binding (rename-binding-exps (cdr bindings)))))))))
		     `(let* ,(rename-binding-exps bindings)
			,@(if (memq old-var vars)
			    bodies
			    (map rename bodies))))))
	       (letrec (decls . bodies)
		 (let ((vars (map car decls))
		       (procs (map cadr decls)))
		   (if (memq old-var vars)
		     exp
		     `(letrec ,(map list vars (map rename procs))
			,@(map rename bodies)))))
	       (set! (var rhs-exp) `(set! ,var ,(rename rhs-exp)))
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
       (rename-in-quasiquote
	 (lambda (datum)
	   (cond
	     ((vector? datum) (list->vector (rename-in-quasiquote (vector->list datum))))
	     ((not (pair? datum)) datum)
	     ;; doesn't handle nested quasiquotes yet
	     ((quasiquote? datum) datum)
	     ((unquote? datum) (list 'unquote (rename (cadr datum))))
	     ((unquote-splicing? datum) (list 'unquote-splicing (rename (cadr datum))))
	     (else (cons (rename-in-quasiquote (car datum))
			 (rename-in-quasiquote (cdr datum)))))))
       (rename-record-case-clause
	 (lambda (clause)
	   (if (eq? (car clause) 'else)
	     (cons 'else (map rename (cdr clause)))
	     (let ((tags (car clause))
		   (formals (cadr clause)))
	       `(,tags ,formals ,@(map rename (cddr clause))))))))
      rename)))

(define contains-unsafe-lambda?
  (lambda (old-var new-var)
    (letrec
      ((unsafe?
	(lambda (exp)
	  (cond
	    ((null? exp) #f)
	    ((literal? exp) #f)
	    ((symbol? exp) #f)
	    (else
	      (record-case exp
		(quote (datum) #f)
		(quasiquote (datum) (unsafe-in-quasiquote? datum))
		(if (test . conseqs) (ormap unsafe? (cdr exp)))
		(cond clauses
		  (ormap (lambda (clause)
			   (if (eq? (car clause) 'else)
			       (ormap unsafe? (cdr clause))
			       (ormap unsafe? clause)))
			 clauses))
	       (lambda (formals . bodies)
		 (cond
		   ((mem? old-var formals) #f)
		   ((and (mem? new-var formals) (memq old-var (all-free bodies '()))) #t)
		   (else (ormap unsafe? bodies))))
	       (let (bindings . bodies) (unsafe? (let-transformer exp)))
	       (let* (bindings . bodies) (unsafe? (let*-transformer exp)))
	       (letrec (decls . bodies) (unsafe? (letrec-transformer exp)))
	       (set! (var rhs-exp) (unsafe? rhs-exp))
	       (begin exps (ormap unsafe? exps))
	       (and exps (ormap unsafe? exps))
	       (or exps (ormap unsafe? exps))
	       (case (kexp . clauses)
		 (or (unsafe? kexp)
		      (ormap (lambda (clause)
			       (if (eq? (car clause) 'else)
				 (ormap unsafe? (cdr clause))
				 (ormap unsafe? clause)))
			      clauses)))
	       (record-case (rexp . clauses)
		 (unsafe? (record-case-transformer exp)))
	       (cases (type rexp . clauses)
		 (unsafe? (record-case-transformer `(record-case ,rexp ,@clauses))))
	       (else (if (memq (car exp) syntactic-keywords)
		       (error 'unsafe? "don't know how to process ~a" exp)
		       ;; application
		       (ormap unsafe? exp))))))))
       (unsafe-in-quasiquote?
	 (lambda (datum)
	   (cond
	     ((vector? datum) (unsafe-in-quasiquote? (vector->list datum)))
	     ((not (pair? datum)) #f)
	     ;; doesn't handle nested quasiquotes yet
	     ((quasiquote? datum) #f)
	     ((unquote? datum) (unsafe? (cadr datum)))
	     ((unquote-splicing? datum) (unsafe? (cadr datum)))
	     (else (or (unsafe-in-quasiquote? (car datum))
		       (unsafe-in-quasiquote? (cdr datum))))))))
      unsafe?)))

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

;;-------------------------------------------------------------------------------

(define all-global-symbols (get-all-global-symbols))

(define datatype-generator
  (make-datatype-generator 'continuation 'cont '(k value) '(v v1 v2 env handler k)))

