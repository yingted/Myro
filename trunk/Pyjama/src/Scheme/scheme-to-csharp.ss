;; temporary - to access various utilities (define*? etc.)
(load "rm-transformer.ss")

(case-sensitive #t)

(define signatures
  '(
    (function void (int int int))
    (fact int (int))
    (Main void ("string []"))
    ;; temporary
    (foo int (int string "long double"))
    ))

(define get-sig
  (lambda (name signatures)
    (cond
     ((null? signatures) '(void ())) ;; no return, no params
     ((eq? name (caar signatures)) (list (cadar signatures)
					 (caddar signatures)))
     (else (get-sig name (cdr signatures))))))

(define convert-file
  (lambda (filename . opt)
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
      (let ((name (proper-name (string->symbol (car (split filename #\.))))))
	(format "public class ~a {\n~a}\n"
		name (apply string-append (map convert-define defs)))))))

(define convert-parameters
  (lambda (name args param-types)
    (db "convert-parameters: ~a(~a) <= ~a ~%" name args param-types)
    (if (not (= (length args) (length param-types)))
	(error 'convert-parameters "Incorrect param sig for '~a'" name)
	(glue (join-list (map (lambda (type arg)
				(format "~a ~a" type (proper-name arg)))
			      param-types args)
			 ", ")))))

(define convert-define
  (lambda (def)
    (db "convert-define: ~a~%" def)
    (let ((name (cadr def)))
      (cond
	((define*? def)
	 (let* ((types (get-sig name signatures))
		(return-type (car types))
		(param-types (cadr types)))
	   ;; def = (define* name (lambda args body ...))
	   (let ((args (cadr (caddr def)))
		 (bodies (cddr (caddr def))))
	     (format "public static ~a ~a(~a) ~a\n"
	       return-type (proper-name name)
	       (convert-parameters name args param-types)
	       (convert-block bodies)))))
	((lambda? (caddr def))
	 ;; primitive function
	 ;; def = (define name (lambda args body ...))
	 (printf "Ignoring primitive function ~a~%" name)
	 "")
	(else
	 ;; def = (define name 'undefined)
	 (format "static object ~a = null;\n" (proper-name name)))))))

(define convert-statement
  (lambda (statement)
    (db "convert-statement: '~s'~%" statement)
    (record-case statement
       (if (test-part . conseqs)  ;true-part false-part)
	   (let ((true-part (car conseqs)))
	     (if (null? (cdr conseqs))
		 (format "if ((bool) (~a)) ~a"
			 (convert-exp test-part)
			 (convert-statement true-part))
		 (let ((false-part (cadr conseqs)))
		   (format "if ((bool) (~a)) ~a else ~a"
			   (convert-exp test-part)
			   (convert-statement true-part)
			   (convert-statement false-part))))))
       (set! (sym exp)
	 (format "~a = ~a;\n" (proper-name sym) (convert-exp exp)))
       (let (bindings . bodies)
	 (let* ((vars (map car bindings))
		(temps (map (lambda (v) (format "object ~a = null;\n"
					  (proper-name v))) vars)))
	   (format "{ ~a ~a }"
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
    (format "{ ~a }" (apply string-append (map convert-statement statements)))))

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
	((+ *) (format "(~a)" (glue (join-list cargs (format " ~a " (proper-name proc))))))
	((- / eq? =)  ;; infix
	 ;; infix: handles only binary procs
	 (format "(~a ~a ~a)" (car cargs) (proper-name proc) (cadr cargs)))
	((and) (format "(~a)" (glue (join-list cargs " && "))))
	((or) (format "(~a)" (glue (join-list cargs " || "))))
	(else
	 (format "~a(~a)"
		 (proper-name proc)
		 (glue (join-list cargs ", "))))))))

(define proper-name
  (lambda (name)
    (cond
     ((eq? name 'eq?) '==)
     ((eq? name '=) '==)
     ((eq? name 'cons) 'Cons)
     ((eq? name '-) '-)
     ((eq? name 'set!) '=)
     ((eq? name 'and) "&&")
     ((eq? name 'or) "||")
     (else (replace (replace (replace name #\- #\_) #\? "_q") #\! "_b")))))

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

;; debug
(define db
  (lambda args
    ;; or nothing to debug off
    ;;(apply printf args)
    'ok
    ))

