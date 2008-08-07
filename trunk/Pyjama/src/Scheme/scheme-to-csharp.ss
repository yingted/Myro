;; temporary - to access various utilities (define*? etc.)
(load "rm-transformer.ss")

(define *system-function-signatures*
  ;; use csharp function names in this format:
  ;; ((function-name return-type (param-types...))...)
  '(
    (error void (string string "object[]"))
    (string_to_integer object (object))
    (string_to_decimal object (object))
    (string_to_rational object (object))
    ))

(define *system-ignore-functions*
  ;; use scheme name of functions to not move to csharp
  '(*function-signatures* *ignore-functions* run trampoline make-cont
	   make-sub string-to-number))

(define convert-file
  (lambda (filename . opt)
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
	(format "~a public class ~a: Scheme {\n~a\n~a}\n"
		header name 
		(apply string-append (map convert-define defs))
		cs-trampoline)))))

(define cs-trampoline
  "    public static object trampoline() {
        while (pc != null) {
            pc();
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

;;     (glue (join-list (map (lambda (type param)
;; 			    (format "~a ~a" type (proper-name param))))
;; 		     types params)
;; 	  ", ")))

(define format-definition
  (lambda (name params)
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
      (format "public static ~a ~a(~a) " 
	      return-type (proper-name name) sparms))))

(define format-application
  (lambda (name args return-cast)
    (let* ((ret-args (get-definition-parameter-types name args 'app))
	   (types (cadr ret-args))
	   (return-type (car ret-args))
	   (args-list (if (symbol? args)
			   (list args)
			   args))
	   (sargs (glue (join-list (map (lambda (type arg)
					  (if (equal? return-cast "") 
					      (format "(~a)~a" type (convert-exp arg))
					      (format "~a" (convert-exp arg))))
					types args-list)
				   ", "))))
      (if (equal? return-cast "") ;; no return override
	  (format "~a(~a) " 
		  ;;return-type 
		  (proper-name name) sargs)
	  (format "return(~a ~a) " return-cast sargs)))))
		  
	   
(define get-definition-parameter-types
  (lambda (name params kind)
    (db "get-definition-parameter-types: ~a(~a) ~%" name params)
    (cond
     ((symbol? params) 
      (if (eq? kind 'def)
	  (lookup-param-types name '("object" ("params object[]")))
	  (lookup-param-types name '("object" ("object[]")))))
     ((null? params) (lookup-param-types name '("void" ())))
     (else (let ((times (length params)))
	     (lookup-param-types name (list "object" (repeat "object" times))))))))

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
	(printf "Ignoring function ~a~%" name)
	"")
       ((not (lambda? (caddr def)))
	;; def = (define name 'undefined)
	(let* ((pname (proper-name name))
	       (types (lookup-signature name 
			(append *function-signatures*
				*system-function-signatures*))))
	  (printf " adding static variable ~a...~%" name)
	  (cond
	   ((equal? pname "pc")
	    (format "static Function pc = null;\n"))
	   ((null? (car types))
	    (format "static object ~a = null;\n" pname))
	   (else
	    (format "static ~a ~a = null;\n" (car types) pname)))))
       ((or (define*? def) (define? def))
	(let ((params (cadr (caddr def)))
	      (bodies (cddr (caddr def))))
	  (db " adding function ~a...~%" name)
	  (format "~a ~a\n" 
		  (format-definition name params)
		  (convert-block bodies))))
       (else
	(error 'convert-define "unrecognized form: ~a" def))))))

(define convert-statement
  (lambda (statement)
    (db "convert-statement: '~s'~%" statement)
    (if (not (pair? statement))
	(format "~a; " statement)
	(record-case statement
	   (if (test-part . conseqs)  ;true-part false-part)
	       (let ((true-part (car conseqs)))
		 (if (null? (cdr conseqs))
		     (format "if (~a) ~a"
			     (convert-exp test-part)
			     (convert-statement true-part))
		     (let ((false-part (cadr conseqs)))
		       (format "if (~a) ~a else ~a"
			       (convert-exp test-part)
			       (convert-statement true-part)
			       (convert-statement false-part))))))
	   (set! (sym exp)
		 (if (eq? sym 'pc)
		     (if (eq? exp #f)
			 "pc = null;\n"
			 (format "pc = (Function) ~a;\n" (proper-name exp)))
		     (format "~a = ~a;\n" (proper-name sym) (convert-exp exp))))
	   (let (bindings . bodies)
	     (let* ((vars (map car bindings))
		    (temps (map (lambda (v) (format "object ~a = null;\n"
						    (proper-name v))) vars)))
	       (format "{\n ~a ~a }\n"
		       (apply string-append temps)
		       (apply string-append (map convert-statement bodies)))))
	   (begin statements
		  (if (null? (cdr statements))
		      (convert-statement (car statements))
		      (convert-block statements)))
	   (else ;; apply (proc args...)
	    (format "~a; " (convert-application (car statement) (cdr statement))))))))
  
(define convert-block
  (lambda (statements)
    (db "convert-block: '~s'~%" statements)
    (format "{\n ~a\n }\n" (apply string-append (map convert-statement statements)))))

(define convert-exp
  (lambda (exp)
    (db "convert-exp: '~s'~%" exp)
    (cond
      ((pair? exp)
       (cond
	 ((eq? (car exp) 'quote) (format "\"~a\"" (cadr exp)))
	 ;; FIXME!
	 ((eq? (car exp) 'quasiquote) (format "\"~a\"" (cadr exp)))
	 ((eq? (car exp) 'if) ;; if expression
	  (format "((~a) ? (~a) : (~a))"
		  (convert-exp (cadr exp)) 
		  (convert-exp (caddr exp)) 
		  (convert-exp (cadddr exp))))
	 (else (convert-application (car exp) (cdr exp)))))
      ((boolean? exp) (if exp "true" "false"))
      ((char? exp)
       (cond
	((char=? exp #\return) "'\\r'")
	((char=? exp #\newline) "'\\n'")
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
      ((string? exp) (format "\"~a\"" exp))
      ((symbol? exp) (format "~a" (proper-name exp)))
      (else (format "~a" exp)))))

(define convert-application
  (lambda (proc args)
    (db "convert-application: ~a(~a)~%" proc args)
    (let ((cargs (map convert-exp args)))
      (case proc
	((and) (format "(~a)" (glue (join-list cargs " && "))))
	((or) (format "(~a)" (glue (join-list cargs " || "))))
	((return)
	 (if (= (length args) 2) ;; extra cast place
	     (format-application proc (cdr args) (car args))
	     (format-application proc args " "))) ;; space will make it a return
	(else (format-application proc args ""))))))

(define proper-name
  (lambda (name)
    (cond
     ((string? name) name)
     ((eq? name 'set!) 'Assign)
     ((eq? name 'eq?) 'Compare)
     ((eq? name 'equal?) 'Compare)
     ((eq? name '<) 'LessThan)
     ((eq? name '>) 'GreaterThan)
     ((eq? name '+) 'Add)
     ((eq? name '=) 'Compare)
     ((eq? name '-) 'Subtract)
     ((eq? name '*) 'Multiply)
     ((eq? name '/) 'Divide)
     ((eq? name 'string) 'str)
     ((eq? name 'operator) 'rator)
     ((eq? name '1st) 'First)
     ((eq? name 'bool) 'logical)
     ((eq? name 'char) 'chr)
     (else (begin (map (lambda (old_new)
			 (set! name (replace name (car old_new) (cadr old_new))))
		       '((#\> "to_")(#\* "_star")(#\= "_is_")
			 (#\- #\_)(#\? "_q")(#\! "_b")(#\/ #\_)))
		  name)))))

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

