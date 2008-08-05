;; temporary - to access various utilities (define*? etc.)
(load "rm-transformer.ss")

(define lookup-signature
  (lambda (name args sigs)
    ;; sigs: '((procname return-type (param-type ...))...)
    (cond
     ((null? sigs) `(void ,(repeat 'object (length args))))
     ((eq? name (caar sigs)) 
      (list (cadar sigs) (caddar sigs)))
     (else (lookup-signature name args (cdr sigs))))))

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



(define repeat
  (lambda (item times)
    (if (= times 0)
	'()
	(cons item (repeat item (- times 1))))))

(define convert-parameters
  (lambda (name args param-types)
    (db "convert-parameters: ~a(~a) <= ~a ~%" name args param-types)
    (glue (join-list (map (lambda (type arg)
			    (format "~a ~a" type (proper-name arg)))
			  param-types args)
		     ", "))))

(define convert-define
  (lambda (def)
    (db "convert-define: ~a~%" def)
    (let ((name (cadr def)))
      (cond
       ((or (equal? name '*function-signatures*)
	    (equal? name '*ignore-functions*)
	    (equal? name 'run)
	    (equal? name 'trampoline)
	    (equal? name 'make-cont)
	    (equal? name 'make-sub)
	    (memq name *ignore-functions*))
	;; primitive function or system function
	;; def = (define name (lambda args body ...))
	(printf "Ignoring function ~a~%" name)
	"")
       ((not (lambda? (caddr def)))
	;; def = (define name 'undefined)
	(let* ((pname (proper-name name))
	       (types (lookup-signature pname '() *function-signatures*)))
	  (printf " adding static variable ~a...~%" name)
	  (cond
	   ((eq? pname 'pc)
	    (format "static Function pc = null;\n"))
	   ((eq? (car types) 'void)
	    (format "static object ~a = null;\n" pname))
	   (else
	    (format "static ~a ~a = null;\n" (car types) pname)))))
       ((or (define*? def) (define? def))
	(let ((args (cadr (caddr def)))
	      (bodies (cddr (caddr def))))
	  (printf " adding function ~a...~%" name)
	  (let* ((types (lookup-signature name args *function-signatures*))
		 (return-type (car types))
		 (param-types (cadr types)))
	    ;; def = (define* name (lambda args body ...))
	    (format "public static ~a ~a(~a) ~a\n"
		    return-type (proper-name name)
		    (convert-parameters name args param-types)
		    (convert-block bodies)))))
       (else
	(error 'convert-define "unrecognized form: ~a" def))))))

(define convert-statement
  (lambda (statement)
    (db "convert-statement: '~s'~%" statement)
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
	(format "~a;\n" (convert-application (car statement) (cdr statement)))))))
  
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
	 (else (convert-application (car exp) (cdr exp)))))
      ((boolean? exp) (if exp "true" "false"))
      ((char? exp)
       (cond
	((char=? exp #\newline) "'\\n'")
	((char=? exp #\space) "' '")
	((char=? exp #\tab) "'\\t'")
	((char=? exp #\nul) "''")
	((char=? exp #\backspace) "'\\h'")
	;; maybe others?
	((char=? exp #\') "'\\''")
	((char=? exp #\") "'\\\"'")
	(else (format "'~a'" exp))))
      ((string? exp) (format "\"~a\"" exp))
      ((symbol? exp) (format "~a" (proper-name exp)))
      (else (format "~a" exp)))))

(define convert-application
  (lambda (proc args)
    (db "convert-application: ~a(~a)~%" proc args)
    (let ((cargs (map convert-exp args)))
      (case proc
;; 	((+ *) (format "(~a)" (glue (join-list cargs (format " ~a " (proper-name proc))))))
;; 	((- / eq? =)  ;; infix
;; 	 ;; infix: handles only binary procs
;; 	 (format "(~a ~a ~a)" (car cargs) (proper-name proc) (cadr cargs)))
	((and) (format "(~a)" (glue (join-list cargs " && "))))
	((or) (format "(~a)" (glue (join-list cargs " || "))))
	(else
	 (format "~a(~a)"
		 (proper-name proc)
		 (glue (join-list cargs ", "))))))))

(define proper-name
  (lambda (name)
    (cond
     ((eq? name 'cons) 'Cons)
     ((eq? name 'set!) 'Assign)
     ((eq? name 'eq?) 'Compare)
     ((eq? name '+) 'Add)
     ((eq? name '=) 'Compare)
     ((eq? name '-) 'Subtract)
     ((eq? name '*) 'Multiply)
     ((eq? name '/) 'Divide)
     (else (begin (map (lambda (old_new)
			 (set! name (replace name (car old_new) (cadr old_new))))
		       '((#\- #\_) (#\? "_q") (#\! "_b")))
		  name)))))

(define glue
  (lambda (things)
    (apply string-append (map (lambda (x) (format "~a" x)) things))))

(define replace
  (lambda (s old new)
    (string->symbol 
     (list->string 
      (sreplace 
       (string->list 
	(symbol->string s)) old new '())))))

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

(define *debug* #t)

;; debug
(define db
  (lambda args
    (if *debug* (apply printf args))
    'ok
    ))

