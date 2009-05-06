;; temporary - to access various utilities (define*? etc.)
(load "rm-transformer.ss")

(define *code* 
"  public static Proc get_variables_from_frame_proc = 
         new Proc(\"get-variables-from-frame\", (Procedure1) get_variables_from_frame, 1, 1);
   public static Proc binding_variable_proc = 
         new Proc(\"binding-variable\", (Procedure1) binding_variable, 1, 1);
")

(define *class* 'PJScheme) ;; 'undefined to use filename

(define *ignore-functions* '(
			     read-content 
			     read-line
			     string->integer 
			     string->decimal 
			     string->rational
			     tagged-list
			     testall
			     group
			     get-current-time
			     range
			     print-parsed-sexps
			     true?
			     type?
			     make-initial-env-extended
			     make_binding_proc
			     first-frame
			     set-first-frame!
			     make-proc
                             make-vector
			     make-binding
			     ))
(define *function-signatures* '())

(define *function-definitions* '())
(define *variable-definitions* '())
(define *applications* '())

(define *system-function-signatures*
  ;; use csharp function names in this format:
  ;; ((function-name return-type (param-types...))...)
  '(
    (symbol<? "bool" ())
    (string<? "bool" ())
    (apply-extension "void" ())
    (apply-cont "void" ())
    (error "void" ("string" "string" "object[]"))
    (scan-string "void" ("object"))
    (scan-file "void" ("object"))
    (scan-string "void" ("object"))
    (scan-file "void" ("object"))
    (apply-state "object" ("object" "object"))
    (read-string "void" ("object"))
    (read-file "void" ("object"))
    (read-next-sexp "void" ("object")) 
    (pc "Function" ("null"))
    (Main "void" ("string []"))
    (make-toplevel-env "object" ())
    (make-macro-env "object" ())
    (make-empty-environment "object" ())
    (display-prim "void" ())
    (pretty-print-prim "void" ())
    (newline-prim "void" ())
    (parse-string "void" ())
    (get-parsed-sexps "void" ())
    (set-car! "void" ())
    (set-cdr! "void" ())
    (set-binding-value! "void" ())
    (set-binding-docstring! "void" ())
    (set-first-frame! "void" ())

    (tagged-list "Func<object,bool>" ("object" "Predicate2" "object"))
    (sort "object" ("Predicate2" "object"))

    (quote? "Func<object,bool>" ())
    (quasiquote? "Func<object,bool>" ())
    (unquote? "Func<object,bool>" ())
    (unquote-splicing? "Func<object,bool>" ())
    (if-then? "Func<object,bool>" ())
    (if-else? "Func<object,bool>" ())
    (assignment? "Func<object,bool>" ())
    (define? "Func<object,bool>" ())
    (define-syntax? "Func<object,bool>" ())
    (begin? "Func<object,bool>" ())
    (lambda? "Func<object,bool>" ())
    (raise? "Func<object,bool>" ())
    (dict? "Func<object,bool>" ())
    (try? "Func<object,bool>" ())
    (catch? "Func<object,bool>" ())
    (finally? "Func<object,bool>" ())

    ))

(define *system-ignore-functions*
  ;; use scheme name of functions to not move to csharp
  '(*function-signatures* *ignore-functions* run trampoline *need-newline*
    *code*))

(define proper-name
  (lambda (name)
    (cond
     ((string? name) name)
     ((eq? name '*need-newline*) "config.NEED_NEWLINE")
     ((eq? name 'apply*) 'apply)
     ((eq? name 'class) 'class_name)
     ((eq? name 'set!) 'Assign)
     ((eq? name 'eq?) 'Eq) 
     ((eq? name '=) 'EqualSign)
     ((eq? name 'equal?) 'Equal)
     ((eq? name 'bool) 'boolean)
     ((eq? name '>) 'GreaterThan)
     ((eq? name '+) 'Add)
     ((eq? name '>=) 'GreaterOrEqual)
     ((eq? name '-) 'Subtract)
     ((eq? name '*) 'Multiply)
     ((eq? name '/) 'Divide)
     ((eq? name 'string) 'make_string)
     ((eq? name 'operator) 'rator)
     ((eq? name '1st) 'First)
     ((eq? name 'char) 'chr)
     (else (begin (map (lambda (old_new)
			 (set! name (replace name (car old_new) (cadr old_new))))
		       '((#\> "to_")(#\< "LessThan")(#\* "_star")(#\= "_is_")
			 (#\- #\_)(#\? "_q")(#\! "_b")(#\/ #\_)))
		  name)))))

(define convert-file
  (lambda (filename . opt)
    (set! *function-definitions* '())
    (set! *variable-definitions* '())
    (set! *applications* '())
    (load filename) ;; to get *function-signatures*
    (call-with-input-file filename
      (lambda (port)
	(let* ((defs (read-defs port))
	       (prog (convert-program defs filename)))
	  (if (null? opt)
	    (printf "~a~%" prog)
	    (call-with-output-file (car opt)
	      (lambda (output-port)
		(fprintf output-port "~a~%" prog)
		(printf "Wrote ~a~%" (car opt))))))))))

(define read-defs
  (lambda (port)
    (let ((sexp (read port)))
      (if (eof-object? sexp)
	  '()
	  (cons sexp (read-defs port))))))

(define convert-program
  (lambda (forms filename)
    (db "convert-program: '~s'~%" forms)
    (let ((defs (filter (lambda (x) (or (define? x) (define*? x))) forms)))
      (let ((name (proper-name (string->symbol (car (split filename #\.)))))
	    (header "using System;\nusing Microsoft.VisualBasic.CompilerServices;\n\n"))
	(if (equal? *class* 'undefined)
	    (set! *class* name))
	(format "~a public class ~a: Scheme {\n~a\n~a ~a}\n"
		header *class*
		(apply string-append (map convert-define defs))
		cs-trampoline
		*code*)))))

(define cs-trampoline
  "      new public static object trampoline () {
	while (pc != null) {
            try {
	        pc ();
	    } catch (Exception e ) {
                if (config.DEBUG > 0) {
                    exception_reg = e.ToString();
                } else {
                    string [] parts = get_parts(e.ToString(), NEWLINE_STRING);
		    exception_reg = format(\"{0}\", parts[0]);
                }
		pc = (Function) apply_handler;
	    }
	}
	return (final_reg);
  }

")

(define lookup-signature
  (lambda (name sigs)
    (db "lookup-signature: '~s'~%" name)
    ;; sigs: '((procname return-type (param-type ...))...)
    (cond
     ((null? sigs) '(()())) ;; no return type, no param types
     ((equal? (format "~a" name) (format "~a" (caar sigs)))
      (list (cadar sigs) (caddar sigs))) ;; return-type (params)
     (else (lookup-signature name (cdr sigs))))))

(define repeat
  (lambda (item times)
    (if (= times 0)
	'()
	(cons item (repeat item (- times 1))))))

(define format-definition
  (lambda (name params)
    (db "format-definition: ~a ~a ~%" name params)
    (let* ((ret-params (get-definition-parameter-types name params 'def))
	   (types (cadr ret-params))
	   (return-type (car ret-params))
	   (param-list (if (symbol? params)
			   (list params)
			   params))
	   (sparms (glue (join-list (map (lambda (type param)
					   (format "~a ~a" type (proper-name param)))
					 types param-list)
				    ", "))))
      (db "return-type: ~a ~%" return-type)
      (format "new public static ~a ~a(~a) " 
	      return-type (proper-name name) sparms))))

(define format-application
  (lambda (name args return-cast proc-name)
    (db "get-definition-parameter-types: ~a(~a) => ~a ~%" name args return-cast)
    (let* ((ret-args (get-definition-parameter-types name args 'app))
	   (proc-return-type (car (get-definition-parameter-types proc-name args 'def)))
	   (types (cadr ret-args))
	   (return-type (car ret-args))
	   (args-list (if (symbol? args)
			   (list args)
			   args))
	   (sargs (glue (join-list (map (lambda (type arg)
					  (if (equal? return-cast "") 
					      (format "(~a)~a" type (convert-exp arg proc-name))
					      (format "~a" (convert-exp arg proc-name))))
					types args-list)
				   ", "))))
      (cond
       ((eq? name 'return*)
	(if (equal? return-cast "") ;; no return override, look it up
	    ;; need to lookup, if void no return just exp
	    (if (equal? proc-return-type "void")
		;; no return, just the function:
		(convert-exp (car args) proc-name)
		(format "return((~a) ~a) " 
			proc-return-type 
			(convert-exp (car args) proc-name)))
	    (format "return((~a) ~a) " 
		    return-cast 
		    sargs)))
       ((or (eq? name 'apply) (eq? name 'map) (eq? name 'for-each) (eq? name 'apply*))
	(let ((sargs (glue (join-list (map (lambda (type arg)
					     (if (equal? return-cast "") 
						 (format "(~a)~a" type (convert-exp arg proc-name))
						 (format "~a" (convert-exp arg proc-name))))
					   (cdr types) (cdr args-list))
				      ", "))))
	  (if (eq? name 'apply*)
	      (format "~a(~a, ~a) " (proper-name name) (convert-exp (car args) proc-name) sargs)
	      (format "~a(~a_proc, ~a) " (proper-name name) (proper-name (car args)) sargs))))
       (else
	(format "~a.~a(~a) " *class* (proper-name name) sargs))))))

(define ends-with
  (lambda (sym c)
    (eq? (car (reverse (string->list (symbol->string sym)))) c)))


(define get-return-type
  (lambda (name params)
    (db "get-return-type: ~a(~a) ~%" name)
    ;; pick a good default
    (let ((return-type (if (ends-with name #\?)
			   "bool"
			   (if (null? params)
			       "void"
			       "object"))))
      (let ((types (lookup-signature name 
		      (append *function-signatures*
			      *system-function-signatures*))))
	(if (null? (car types))
	    return-type
	    (car types))))))

(define get-definition-parameter-types
  (lambda (name params kind)
    (db "get-definition-parameter-types: ~a(~a) ~%" name params)
    (if (pair? name)
	(list '() '())
	(let ((return-type (if (ends-with name #\?)
			       "bool"
			       "object")))
	  (cond
	   ((symbol? params) 
	    (if (eq? kind 'def)
		(lookup-param-types name (list return-type '("params object[]")))
		(lookup-param-types name (list return-type '("object[]")))))
	   ((null? params) (lookup-param-types name '("void" ())))
	   (else (let ((times (length params)))
		   (lookup-param-types name 
				       (list return-type (repeat "object" times))))))))))

(define lookup-param-types
  (lambda (name defaults)
    (let* ((sig (lookup-signature name 
		  (append *function-signatures*
			  *system-function-signatures*)))
	   (return-type (if (null? (car sig))
			    (car defaults)
			    (car sig)))
	   (param-types (if (null? (cadr sig))
			    (cadr defaults)
			    (cadr sig))))
      (list return-type param-types))))

(define convert-define
  (lambda (def)
    (db "convert-define: ~a~%" def)
    (let ((name (cadr def)))
      (cond
       ((memq name (append *ignore-functions* *system-ignore-functions*))
	;; primitive function or system function
	;; def = (define name (lambda args body ...))
	;;(printf "Ignoring function ~a~%" name)
	"")
       ((not (lambda? (caddr def)))
	;; def = (define name 'undefined)
	(let* ((pname (proper-name name))
	       (types (lookup-signature name 
			(append *function-signatures*
				*system-function-signatures*))))
	  (if (eq? (memq name *variable-definitions*) #f)
	      (begin
		;;(printf " adding static variable ~a...~%" name)
		(set! *variable-definitions* (cons name *variable-definitions*))
		(let ((ret-type (if (null? (car types))
				    (if (ends-with name #\?)
					"bool"
					"object")
				    (car types)))
		      (assign-exp (if (null? (cadr types))
				      (convert-exp (caddr def) name)
				      (caadr types))))
		  (format "static ~a ~a = ~a;\n" ret-type pname assign-exp)))
	      (begin
		(printf "skipping duplicate variable definition: ~a~%" name)
		""))))
       ((or (define*? def) (define? def))
	(let ((params (cadr (caddr def)))
	      (bodies (cddr (caddr def))))
	  ;;(printf " adding function ~a...~%" name)
	  (set! *function-definitions* (cons name *function-definitions*))
	  (format "~a ~a\n" 
		  (format-definition name params)
		  (convert-block bodies name))))
       (else
	(error 'convert-define "unrecognized form: ~a" def))))))

(define convert-statement
  (lambda (statement proc-name)
    (db "convert-statement: '~s'~%" statement)
    (if (not (pair? statement))
	(format "~a; " statement)
	(record-case statement
	   (if (test-part . conseqs)  ;true-part false-part)
	       (let ((true-part (car conseqs)))
		 (if (null? (cdr conseqs))
		     (format "if (true_q(~a)) ~a"
			     (convert-exp test-part proc-name)
			     (convert-statement true-part proc-name))
		     (let ((false-part (cadr conseqs)))
		       (format "if (true_q(~a)) ~a else ~a"
			       (convert-exp test-part proc-name)
			       (convert-statement true-part proc-name)
			       (convert-statement false-part proc-name))))))
	   (set! (sym exp)
		 (if (eq? sym 'pc)
		     (if (eq? exp #f)
			 "pc = null;\n"
			 (format "pc = (Function) ~a;\n" (proper-name exp)))
		     (format "~a = ~a;\n" (proper-name sym) (convert-exp exp proc-name))))
	   (let (bindings . bodies)
	     (let* ((vars (map car bindings))
		    (temps (map (lambda (v) (format "object ~a = null;\n"
						    (proper-name v))) vars)))
	       (format "{\n ~a ~a }\n"
		       (apply string-append temps)
		       (apply string-append 
			      (map 
			       (lambda (exp) (convert-statement exp proc-name))
			       bodies)))))
	   (begin statements
		  (if (null? (cdr statements))
		      (convert-statement (car statements) proc-name)
		      (convert-block statements proc-name)))
	   (else ;; apply (proc args...)
	    (format "~a; " (convert-application (car statement) 
						(cdr statement) proc-name)))))))
  
(define convert-block
  (lambda (statements proc-name)
    (db "convert-block: '~s'~%" statements)
    (format "{\n ~a\n }\n" (apply string-append 
				  (map (lambda (s)
					 (convert-statement s proc-name))
				       statements)))))

(define convert-exp
  (lambda (exp proc-name)
    (db "convert-exp: '~s'~%" exp)
    (cond
      ((null? exp) "EmptyList")
      ((pair? exp)
       (cond
	 ((eq? (car exp) 'quote) 
	  (if (eq? (cadr exp) '())
	      "EmptyList"
	      (format "symbol(\"~a\")" (cadr exp))))
	 ((eq? (car exp) 'quasiquote) (format "\"~a\"" (cadr exp)))
	 ((eq? (car exp) 'if) ;; if expression
	  (format "((~a) ? (~a) : (~a))"
		  (convert-exp (cadr exp) proc-name) 
		  (convert-exp (caddr exp) proc-name) 
		  (convert-exp (cadddr exp) proc-name)))
	 (else (convert-application (car exp) (cdr exp) proc-name))))
      ((boolean? exp) (if exp "true" "false"))
      ((char? exp)
       (cond
	((char=? exp #\return) "'\\r'")
	((char=? exp #\newline) "NEWLINE")
	((char=? exp #\space) "' '")
	((char=? exp #\tab) "'\\t'")
	((char=? exp #\nul) "NULL")
	((char=? exp #\backspace) "BACKSPACE")
	((char=? exp #\\) "BACKSLASH")
	((char=? exp #\`) "BACKQUOTE")
	;; maybe others?
	((char=? exp #\") "DOUBLEQUOTE") 
	((char=? exp #\') "SINGLEQUOTE")
	((char=? exp #\~) "TILDE")
	(else (format "'~a'" exp))))
      ((string? exp) (format "\"~a\"" (replace exp #\newline "\\n")))
      ((symbol? exp) (format "~a" (proper-name exp)))
      (else (format "~a" exp)))))

(define convert-application
  (lambda (name args proc-name)
    (db "convert-application: ~a(~a) in ~a~%" name args proc-name)
    (if (not (member name *applications*))
	(set! *applications* (cons name *applications*)))
    (let ((bool-cargs (map (lambda (e) (format "((bool)~a)" (convert-exp e proc-name))) args))
	  (cargs (map (lambda (e) (convert-exp e proc-name)) args)))
      (case name
	((error) (format "throw new Exception(format(~a + \": \" + ~a, ~a))"
			 (car cargs)
			 (cadr cargs)
			 (caddr cargs)))
	((and) (format "(~a)" (glue (join-list bool-cargs " && "))))
	((or) (format "(~a)" (glue (join-list bool-cargs " || "))))
	((return*)
	 (if (= (length args) 2) ;; explicit cast in return
	     (format-application name (cdr args) (car args) proc-name)
	     (format-application name args "" proc-name))) ;; lookup cast
	(else (format-application name args "" proc-name))))))

(define glue
  (lambda (things)
    (apply string-append (map (lambda (x) (format "~a" x)) things))))

(define replace
  (lambda (s old new)
    (list->string 
     (sreplace 
      (string->list 
       (format "~a" s)) old new '()))))

(define string->chars
  (lambda (s)
    (cond
     ((char? s) (list s))
     ((string? s) (string->list s))
     ((symbol? s) (string->list (symbol->string s)))
     (else (error 'string->chars "unknown type: ~a" s)))))

(define sreplace
  (lambda (lyst old new accum)
    (cond
     ((null? lyst) (reverse accum))
     ((eq? (car lyst) old)
      (sreplace (cdr lyst) old new
		(append (reverse (string->chars new)) accum)))
     (else (sreplace (cdr lyst) old new
		     (cons (car lyst) accum))))))

(define split
  (lambda (s delim)
    (let ((list (string->list s)))
      (reverse (split-help delim list '() '())))))

(define split-clean
  (lambda (items)
    (cond
     ((null? items) ())
     (else (cons (list->string (reverse (car items)))
		 (split-clean (cdr items)))))))

(define split-help
  (lambda (delim in buffer out)
    (cond 
     ((null? in) (split-clean (if (null? buffer)
				  out
				  (cons buffer out))))
     ((eq? (car in) delim) 
      (split-help delim (cdr in) '() (cons buffer out)))
     (else (split-help delim (cdr in) (cons (car in) buffer) out)))))

(define join-list
  (lambda (lyst delim)
    (cond
     ((null? lyst) '())
     ((null? (cdr lyst)) lyst)
     (else (cons (car lyst) (cons delim (join-list (cdr lyst) delim)))))))

(define *debug* #f)

;; debug
(define db
  (lambda args
    (if *debug* (apply printf args))
    'ok
    ))

