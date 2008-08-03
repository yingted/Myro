(load "ds-transformer.ss")

(define *remove-record-case?* #t)

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

(define sort-assignments
  (lambda (assigns)
    (let ((graph (make-graph assigns)))
      (if (contains-cycle? graph)
	(make-temp-assignments assigns)
	(map vertex-contents (topological-sort graph))))))

(define get-neighbors
  (lambda (v graph)
    (filter (lambda (u)
	      (and (not (eq? v u))
		   (let* ((assign1 (vertex-contents v))
			  (assign2 (vertex-contents u))
			  (free-vars1 (free assign1 '()))
			  (var2 (cadr assign2)))
		     (memq var2 free-vars1))))
	    graph)))

(define make-vertex (lambda (x) (cons 'unmarked x)))
(define vertex-contents cdr)
(define vertex-tag? (lambda (v tag) (eq? (car v) tag)))
(define mark-vertex! set-car!)

(define make-graph (lambda (values) (map make-vertex values)))

(define mark-all-vertices!
  (lambda (graph tag)
    (for-each (lambda (v) (mark-vertex! v tag)) graph)))

(define make-stack (lambda () (cons 'stack '())))
(define stack-contents cdr)
(define push-stack! (lambda (x s) (set-cdr! s (cons x (cdr s)))))

(define contains-cycle?
  (lambda (graph)
    (call/cc
      (lambda (return)
	(letrec
	  ((visit-vertex
	     (lambda (v)
	       (mark-vertex! v 'grey)
	       (for-each
		 (lambda (u)
		   (cond
		     ((vertex-tag? u 'grey) (return #t))
		     ((vertex-tag? u 'white) (visit-vertex u))))
		 (get-neighbors v graph))
	       (mark-vertex! v 'black))))
	  (mark-all-vertices! graph 'white)
	  (for-each
	    (lambda (v)
	      (if (vertex-tag? v 'white)
		  (visit-vertex v)))
	    graph)
	  #f)))))

(define topological-sort
  (lambda (graph)
    (let ((stack (make-stack)))
      (letrec
	((depth-first-search
	   (lambda (v)
	     (mark-vertex! v 'visited)
	     (for-each
	       (lambda (neighbor)
		 (if (not (vertex-tag? neighbor 'visited))
		     (depth-first-search neighbor)))
	       (get-neighbors v graph))
	     (push-stack! v stack))))
	(mark-all-vertices! graph 'unvisited)
	(for-each
	  (lambda (v)
	    (if (not (vertex-tag? v 'visited))
		(depth-first-search v)))
	  graph)
	(stack-contents stack)))))

(define make-temp-vars
  (lambda (num exps)
    (let ((free-vars (all-free exps '())))
      (letrec
	((make-safe-temps
	   (lambda (i temps)
	     (if (= (length temps) num)
	       (reverse temps)
	       (let ((sym (string->symbol (format "temp_~a" i))))
		 (if (memq sym free-vars)
		   (make-safe-temps (+ i 1) temps)
		   (make-safe-temps (+ i 1) (cons sym temps))))))))
	(let ((temp-vars (make-safe-temps 1 '())))
	  (register-table 'add-temps temp-vars)
	  temp-vars)))))

(define make-temp-assignments
  (lambda (assigns)
    (let* ((registers (map cadr assigns))
	   (exps (map caddr assigns))
	   (temp-vars (make-temp-vars (length assigns) assigns))
	   (temp-assigns (map (lambda (t e) `(set! ,t ,e)) temp-vars exps))
	   (reg-assigns (map (lambda (r t) `(set! ,r ,t)) registers temp-vars)))
      (append temp-assigns reg-assigns))))

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


