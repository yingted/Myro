(load "ds-transformer.ss")

;; assumes:
;; - code in first-order tail form
;; - define*
;; - no internal define/define*'s
;; - no mit-style define/define*'s

;;-------------------------------------------------------------------------------

(define get-defined-symbols
  (lambda (source-file)
    (call-with-input-file source-file
      (lambda (port)
	(let loop ((exp (read port)) (syms '()) (syms* '()))
	  (cond
	    ((eof-object? exp) (cons syms syms*))
	    ((define? exp)
	     (if (mit-style? exp)
		 (loop (read port) (union (caadr exp) syms) syms*)
		 (loop (read port) (union (cadr exp) syms) syms*)))
	    ((define*? exp)
	     (if (mit-style? exp)
		 (loop (read port) syms (union (caadr exp) syms*))
		 (loop (read port) syms (union (cadr exp) syms*))))
	    ((eopl-define-datatype? exp)
	     (loop (read port) (union (map car (cdddr exp)) syms) syms*))
	    (else (loop (read port) syms syms*))))))))

(define get-all-defined-symbols
  (lambda (source-files)
    (union-all (map get-defined-symbols source-files))))

(define register-transform-file
  (lambda (source-file . opts)
    (let* ((eopl-defs '())
	   (procedure-defs '())
	   (other-defs '())
	   (all-defined-syms (get-defined-symbols source-file))
	   (syms (car all-defined-syms))
	   (syms* (cdr all-defined-syms)))
      (letrec
	((read-transformed-exps
	   (lambda (input-port)
	     (let ((exp (read input-port)))
	       (printf "read ~s~%" exp)
	       (cond
	         ((eof-object? exp)
		  (set! other-defs (reverse other-defs)))
		 ;; skip top level calls to load
		 ((load? exp) (read-transformed-exps input-port))
		 ;; must transform the expression before calling the recursion
		 (else (let ((texp ((register-transform syms*) exp)))
			 (cond
			   ((eopl-define-datatype? texp)
			    (set! eopl-defs (cons texp eopl-defs)))
			   ((procedure-definition? texp)
			    (set! procedure-defs (cons texp procedure-defs)))
			   (else (set! other-defs (cons texp other-defs))))
			 (read-transformed-exps input-port)))))))
	 (print-exp
	   (lambda (output-port)
	     (lambda (exp)
	       (pretty-print exp output-port)
	       (newline output-port))))
	 (print-code
	   (lambda (output-port)
	     ;; did we see a define-datatype or cases form?
	     (if need-eopl-support?
	       (begin
		 (fprintf output-port ";;~a~%" (make-string 70 #\-))
		 (fprintf output-port ";; EOPL support~%~%")
		 (fprintf output-port "(load \"petite-init.ss\")~%")
		 (fprintf output-port "(load \"define-datatype.ss\")~%~%")
		 (for-each (print-exp output-port) eopl-defs)))
	     ;; write register machine code
	     (fprintf output-port ";;~a~%" (make-string 70 #\-))
	     (fprintf output-port ";; main program~%~%")
	     (for-each (print-exp output-port) procedure-defs)
	     (for-each (print-exp output-port) other-defs))))
	(call-with-input-file source-file
	  (lambda (input-port)
	    (read-transformed-exps input-port)))
	(if (null? opts)
	  (begin
	    (newline)
	    (print-code (current-output-port)))
	  (let ((output-filename (car opts)))
	    (call-with-output-file output-filename
	      (lambda (output-port)
		(print-code output-port)
		(printf "Output written to ~a~%" output-filename)))))))))

(define register-transform
  (lambda (syms*)
    (letrec
       ((transform
	  (lambda (code)
	    (cond
	     ((null? code) code)
	     ((literal? code) code)
	     ((symbol? code) code)
	     (else
	      (record-case code
		(quote (datum) code)
		(quasiquote (datum)
		  (list 'quasiquote (transform-quasiquote datum)))
		(if (test . conseqs)
		  `(if ,@(map transform (cdr code))))
		(cond clauses
		  `(cond ,@(map (lambda (clause)
				  (if (eq? (car clause) 'else)
				      `(else ,@(map transform (cdr clause)))
				      (map transform clause)))
				clauses)))
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
		  (let ((vars (map car decls))
			(procs (map cadr decls)))
		    `(letrec ,(map list vars (map transform procs))
		       ,@(map transform bodies))))
		(set! (var rhs-exp)
		  `(set! ,var ,(transform rhs-exp)))
		(begin exps
		  `(begin ,@(map transform exps)))
		((define define*) (name . bodies)
		 (cond
		   ((mit-style? code)
		    (error-in-source code "MIT-style define not allowed."))
		   ((lambda? (car bodies))
		    (let* ((lambda-exp (car bodies))
			   (formals (cadr lambda-exp))
;; formals could be an improper list
			   (registers (get-register-names formals)))
		      (if (memq name syms*)
			;; define*
			(let* ((new-lambda (rename-lambda-formals registers lambda-exp))
			       (new-bodies (cddr new-lambda)))
			  `(define ,name
			     (lambda ()
			       ,@(map transform new-bodies))))
			;; define
			(let ((lambda-bodies (cddr lambda-exp)))
			  `(define ,name
			     (lambda ,formals
			       ,@(map transform lambda-bodies)))))))
		   (else `(define ,name ,(transform (car bodies))))))
		(define-syntax args code)
		(and exps
		  `(and ,@(map transform exps)))
		(or exps
		  `(or ,@(map transform exps)))
		(case (exp . clauses)
		  `(case ,(transform exp)
		     ,@(transform-case-clauses clauses)))
		(record-case (exp . clauses)
		  `(record-case ,(transform exp)
		     ,@(transform-rc-clauses clauses)))
		;; EOPL
		(define-datatype args
		  (set! need-eopl-support? #t)
		  code)
		(cases (type exp . clauses)
		  (set! need-eopl-support? #t)
		  `(cases ,type ,(transform exp)
		     ,@(transform-rc-clauses clauses)))
		(else (if (memq (car code) syntactic-keywords)
			  (error-in-source code
			    "I don't know how to process the above code.")
			  (map transform code))))))))
	(transform-quasiquote
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
	(transform-rc-clauses
	  (lambda (clauses)
	    (map (lambda (clause)
		   (if (eq? (car clause) 'else)
		       `(else ,@(map transform (cdr clause)))
		       (let ((tags (car clause))
			     (formals (cadr clause))
			     (bodies (cddr clause)))
			 `(,tags ,formals ,@(map transform bodies)))))
		 clauses)))
	(transform-case-clauses
	  (lambda (clauses)
	    (map (lambda (clause) (cons (car clause) (map transform (cdr clause))))
		 clauses))))
       transform)))

(define get-register-names
  (lambda (params)
    (map (lambda (x) (string->symbol (string-append (symbol->string x) "_reg")))
	 params)))
