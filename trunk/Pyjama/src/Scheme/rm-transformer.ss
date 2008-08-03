(load "ds-transformer.ss")

;; assumes:
;; - code in first-order tail form
;; - define*
;; - no internal define/define*'s
;; - no mit-style define/define*'s

;;-------------------------------------------------------------------------------

;;(define get-define*-symbols
;;  (lambda (source-file)
;;    (call-with-input-file source-file
;;      (lambda (port)
;;	(let loop ((exp (read port)) (syms* '()))
;;	  (cond
;;	    ((eof-object? exp) syms*)
;;	    ((define? exp) (loop (read port) syms*))
;;	    ((define*? exp)
;;	     (if (mit-style? exp)
;;		 (loop (read port) (union (caadr exp) syms*))
;;		 (loop (read port) (union (cadr exp) syms*))))
;;	    ((eopl-define-datatype? exp) (loop (read port) syms*))
;;	    (else (loop (read port) syms*))))))))

;;(define get-all-define*-symbols
;;  (lambda (source-files)
;;    (union-all (map get-define*-symbols source-files))))

;;(define get-defined-symbols
;;  (lambda (source-file)
;;    (call-with-input-file source-file
;;      (lambda (port)
;;	(let loop ((exp (read port)) (syms '()) (syms* '()))
;;	  (cond
;;	    ((eof-object? exp) (cons syms syms*))
;;	    ((define? exp)
;;	     (if (mit-style? exp)
;;		 (loop (read port) (union (caadr exp) syms) syms*)
;;		 (loop (read port) (union (cadr exp) syms) syms*)))
;;	    ((define*? exp)
;;	     (if (mit-style? exp)
;;		 (loop (read port) syms (union (caadr exp) syms*))
;;		 (loop (read port) syms (union (cadr exp) syms*))))
;;	    ((eopl-define-datatype? exp)
;;	     (loop (read port) (union (map car (cdddr exp)) syms) syms*))
;;	    (else (loop (read port) syms syms*))))))))

;;(define get-all-defined-symbols
;;  (lambda (source-files)
;;    (union-all (map get-defined-symbols source-files))))

(define make-register-table
  (lambda ()
    (let ((all-registers '(final_reg))
	  (registerized-functions '())
	  (temp-vars '()))
      (lambda msg
	(record-case msg
	  (info ()
	    (printf "registers:~%")
	    (for-each (lambda (r) (printf "   ~a~%" r)) all-registers)
	    (printf "temporary vars:~%")
	    (for-each (lambda (r) (printf "   ~a~%" r)) temp-vars)
	    (for-each (lambda (f) (printf "~a: ~a~%" (car f) (cdr f)))
		      registerized-functions))
	  (get-registers () (sort alphabetical? all-registers))
	  (get-temp-vars () temp-vars)
	  (registerize-application? (code)
	    (and (symbol? (car code))
		 (assq (car code) registerized-functions)))
	  (registerize (code)
	    (let* ((operator (car code))
		   (operands (cdr code))
		   (params (cdr (assq operator registerized-functions)))
		   (assigns (params->assignments params operands))
		   (final-assigns (sort-assignments (remove-redundancies assigns)))
		   (pc-assign `(set! pc ,operator)))
	      `(begin ,@final-assigns ,pc-assign)))
	  (add-function (name regs)
	    (if (assq name registerized-functions)
	      (error-in-source #f
		(format "Symbol ~a is defined more than once at top level." name)))
	    (set! all-registers (union regs all-registers))
	    (set! registerized-functions (cons (cons name regs) registerized-functions))
	    'ok)
	  (add-temps (temps)
	    (set! temp-vars (union temps temp-vars))
	    'ok)
	  (else (error 'register-table "bad message: ~a" msg)))))))

(define register-table (make-register-table))

(define reg-transform-file
  (lambda (source-file . opts)
    (set! register-table (make-register-table))
    (let ((eopl-defs '())
	  (defs '()))
      (letrec
	((transform-definitions
	   (lambda (input-port)
	     (let ((exp (read input-port)))
	       (if (eof-object? exp)
		 (begin
		  (set! eopl-defs (reverse eopl-defs))
		  (set! defs (reverse (map reg-transform defs))))
		 (begin
		   (cond
		     ((or (define? exp) (define*? exp))
		      (set! defs (cons (preprocess-define exp) defs)))
		     ((eopl-define-datatype? exp)
		      (set! eopl-defs (cons exp eopl-defs))
		      (set! need-eopl-support? #t))
		     ;; skip top level calls to load
		     (else 'skip))
		   (transform-definitions input-port))))))
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
	     ;; global registers
	     (fprintf output-port ";;~a~%~%" (make-string 70 #\-))
	     (fprintf output-port ";; global registers~%")
	     (fprintf output-port "(define pc 'undefined)~%")
	     (for-each
	       (lambda (r) (fprintf output-port "(define ~a 'undefined)~%" r))
	       (register-table 'get-registers))
	     (newline output-port)
	     ;; temporary registers
	     (if (not (null? (register-table 'get-temp-vars)))
	       (begin
		 (fprintf output-port ";; temporary registers~%")
		 (for-each
		   (lambda (t) (fprintf output-port "(define ~a 'undefined)~%" t))
		   (register-table 'get-temp-vars))
		 (newline output-port)))
	     ;; registerized function definitions
	     (for-each (print-exp output-port) defs)
	     ;; trampoline
	     (fprintf output-port ";; the trampoline~%")
	     (pretty-print
	       '(define trampoline
		  (lambda ()
		    (if pc
		      (begin
			(pc)
			(trampoline))
		      final_reg)))
	       output-port)
	     (newline output-port)
	     (pretty-print
	       '(define run
		  (lambda (setup . args)
		    (apply setup args)
		    (trampoline)))
	       output-port)
	     (newline output-port))))
	(call-with-input-file source-file
	  (lambda (input-port)
	    (transform-definitions input-port)))
	(if (null? opts)
	  (begin
	    (newline)
	    (print-code (current-output-port)))
	  (let ((output-filename (car opts)))
	    (call-with-output-file output-filename
	      (lambda (output-port)
		(print-code output-port)
		(printf "Output written to ~a~%" output-filename)))))))))

(define preprocess-define
  (lambda (def)
    (cond
      ((mit-style? def) (preprocess-define (mit-define->define def)))
      ((define*? def)
       (let* ((name (cadr def))
	      (lambda-exp (caddr def))
	      (formals (cadr lambda-exp))
	      (registers (get-register-names formals))
	      (new-lambda (rename-lambda-formals registers lambda-exp))
	      (new-bodies (cddr new-lambda)))
	 (register-table 'add-function name registers)
	 `(define ,name (lambda () ,@new-bodies))))
      (else def))))

(define reg-transform
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
	    (define (name body)
	      `(define ,name ,(transform body)))
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
		 ,@(transform-record-case-clauses clauses)))
	    ;; EOPL (should never see a define-datatype at this point)
	    (cases (type exp . clauses)
	      (set! need-eopl-support? #t)
	      `(cases ,type ,(transform exp)
		 ,@(transform-record-case-clauses clauses)))
	    (halt* (value)
	      `(begin (set! final_reg ,value) (set! pc #f)))
	    (else (cond
		    ((memq (car code) syntactic-keywords)
		     (error-in-source code "I don't know how to process the above code."))
		    ((register-table 'registerize-application? code)
		     (register-table 'registerize code))
		    (else (map transform code)))))))))
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
     (transform-record-case-clauses
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
    transform))

;; a1 = (set! x (..... y .....))
;; a2 = (set! y (..... z .....))

;;(define sort-assignments
;;  (lambda (assigns)
;;    (call/cc
;;      (lambda (return)
;;	(sort (lambda (a1 a2)
;;		(let ((var1 (cadr a1))
;;		      (free-vars1 (free (caddr a1) '()))
;;		      (var2 (cadr a2))
;;		      (free-vars2 (free (caddr a2) '())))
;;		  (cond
;;		   ((and (memq var1 free-vars2) (memq var2 free-vars1))
;;		    (make-temp-assignments assigns return))
;;		   ((memq var1 free-vars2) #f)
;;		   ((memq var2 free-vars1) #t)
;;		   (else #t))))
;;	      assigns)))))

(define sort-assignments
  (lambda (assigns)
    (let ((graph (map node assigns)))
      (if (contains-cycle? graph)
	(make-temp-assignments assigns)
	(map cdr (cdr (topological-sort graph)))))))

(define node (lambda (x) (cons 'unvisited x)))
(define a1 '(set! y (list a b x 'a1)))
(define a2 '(set! x (list 1 2 3 'a2)))
(define a3 '(set! z (list x 'a3)))
(define a4 '(set! x (list z 'a4)))
(define n1 (node a1))
(define n2 (node a2))
(define n3 (node a3))
(define n4 (node a4))

(define graph1 (list n1 n2 n3))
(define graph2 (list n3 n4))

(define mark! set-car!)
(define value cdr)
(define visited? (lambda (x) (eq? (car x) 'visited)))
(define tag? (lambda (n tag) (eq? (car n) tag)))
(define push!
  (lambda (x s)
    (set-cdr! s (cons x (cdr s)))))

;; a1 --> a2
;; a3 --> a2

(define neighbors
  (lambda (n1 nodes)
    (filter (lambda (n2)
	      (and (not (eq? n1 n2))
		   (let ((free-vars1 (free (caddr (value n1)) '()))
			 (var2 (cadr (value n2))))
		     (memq var2 free-vars1))))
	    nodes)))

(define mark-all
  (lambda (nodes tag)
    (for-each (lambda (n) (mark! n tag)) nodes)))

(define contains-cycle?
  (lambda (nodes)
    (call/cc
      (lambda (return)
	(mark-all nodes 'white)
	(for-each (lambda (v)
		    (if (tag? v 'white)
			(if (visit v nodes)
			    (return #t))))
	  nodes)
	#f))))

(define visit
  (lambda (v nodes)
    (call/cc
      (lambda (return)
	(mark! v 'grey)
	(for-each
	 (lambda (u)
	   (if (tag? u 'grey)
	       (return #t)
	       (if (tag? u 'white)
		 (if (visit u nodes)
		   (return #t)))))
	 (neighbors v nodes))
	(mark! v 'black)
	#f))))

(define topological-sort
  (lambda (nodes)
    (mark-all nodes 'unvisited)
    (let ((stack (list 'stack)))
      (for-each
	(lambda (n)
	  (if (not (visited? n))
	    (topological-helper n nodes stack)))
	nodes)
      stack)))

(define topological-helper
  (lambda (n nodes stack)
    (mark! n 'visited)
    (for-each
      (lambda (neighbor)
	(if (not (visited? neighbor))
	    (topological-helper neighbor nodes stack)))
      (neighbors n nodes))
    (push! n stack)))

(define make-temp-assignments
  (lambda (assigns)
    (let ((registers (map cadr assigns))
	  (exps (map caddr assigns))
	  (free-vars (all-free assigns '())))
      (letrec
	((make-safe-temps
	   (lambda (i temps)
	     (if (= (length temps) (length assigns))
	       (reverse temps)
	       (let ((sym (string->symbol (format "temp_~a" i))))
		 (if (memq sym free-vars)
		   (make-safe-temps (+ i 1) temps)
		   (make-safe-temps (+ i 1) (cons sym temps))))))))
	(let* ((temps (make-safe-temps 1 '()))
	       (temp-assigns (map (lambda (t e) `(set! ,t ,e)) temps exps))
	       (reg-assigns (map (lambda (r t) `(set! ,r ,t)) registers temps)))
	  (register-table 'add-temps temps)
	  (append temp-assigns reg-assigns))))))

(define remove-redundancies
  (lambda (assigns)
    (filter (lambda (a) (not (eq? (cadr a) (caddr a))))
	    assigns)))

(define param->assign
  (lambda (param operand)
    `(set! ,param ,operand)))

;; params could be an improper list
(define params->assignments
  (lambda (params operands)
    (cond
      ((null? params) '())
      ((symbol? params) (list (param->assign params `(list ,@operands))))
      (else (cons (param->assign (car params) (car operands))
		  (params->assignments (cdr params) (cdr operands)))))))

(define param->reg-name
  (lambda (param)
    (string->symbol (format "~a_reg" param))))

;; params could be an improper list
(define get-register-names
  (lambda (params)
    (cond
      ((null? params) '())
      ((symbol? params) (param->reg-name params))
      (else (cons (param->reg-name (car params)) (get-register-names (cdr params)))))))


