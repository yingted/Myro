;; temporary - to access various utilities (define*? etc.)
(load "rm-transformer.ss")

(case-sensitive #t)

;; for testing
(define get-sexps
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
	(read-defs port)))))

(define convert-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
	(let* ((defs (read-defs port))
	       (prog (convert-program defs filename)))
	  prog)))))

(define convert-program
  (lambda (defs filename)
    (db "convert-program: '~s'~%" defs)
    (let ((name (proper-name (string->symbol (car (split filename #\.))))))
      `(public class ,name #\{ #\newline
	       ,@(flatmap convert-define defs)
	       #\} #\newline))))

(define flatmap
  (lambda (f l)
    (apply append (map f l))))

(define flatten
  (lambda (l)
    (cond
     ((not (list? l)) (list l))
     ((null? l) '())
     (else (apply append (map flatten l))))))

(define read-defs
  (lambda (port)
    (let ((sexp (read port)))
      (if (eof-object? sexp)
	  '()
	  (cons sexp (read-defs port))))))

;; debug
(define db
  (lambda args
    ;; or nothing to debug off
    ;;(apply printf args)
    'ok
    ))

(define proper-name
  (lambda (name)
    (cond
     ((eq? name 'eq?) '==)
     ((eq? name '=) '==)
     ((eq? name 'cons) 'Cons)
     ((eq? name '-) '-)
     ((eq? name 'set!) '=)
     (else (replace (replace name #\- #\_) #\? "_q")))))

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
     (else (cons (car lyst) (cons #\, (join-list (cdr lyst) delim)))))))

(define convert-case
  (lambda (cases)
    (db "convert-case: '~s'~%" cases)
    (cond
     ((null? cases) '())
     (else 
      (append
       (list
	'if #\( (convert-exp (caar cases)) #\)
	(convert-block (cdar cases))
	(convert-case (cdr cases))))))))

(define convert-block
  (lambda (statements)
    (db "convert-block: '~s'~%" statements)
    (if (eq? (car statements) 'begin)
	(convert-block (cdr statements))
	`(#\{ #\newline ,@(flatmap convert-statement statements) #\} #\newline))))

(define signatures
  '(
    (function void (int int int))
    (fact int (int))
    (Main void ("string []"))
    ))

(define get-sig
  (lambda (name signatures)
    (cond
     ((null? signatures) '(void ())) ;; no return, no params
     ((eq? name (caar signatures)) (list (cadar signatures)
					 (caddar signatures)))
     (else (get-sig name (cdr signatures))))))

(define convert-parameters
  (lambda (name args param-types)
    (db "convert-parameters: ~a(~a) <= ~a ~%" name args param-types)
    (if (not (eq? (length args) (length param-types)))
	(error 'convert-parameters "Incorrect param sig for '~a'" name)
	(join-list (map (lambda (type arg)
			  (format "~a ~a" type (proper-name arg)))
			param-types args)
		   #\,))))

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
	     `(public static ,return-type ,(proper-name name)
		      #\( ,@(convert-parameters name args param-types) #\) 
		      ,@(convert-block bodies)))))
	((and (> (length def) 2) (lambda? (caddr def)))
	 ;; primitive function
	 ;; def = (define name (lambda args body ...))
	 (printf "Ignoring primitive function ~a~%" name)
	 '())
	((define? def)
	 ;; def = (define name value)
	 `(static object ,(proper-name name) = null #\; #\newline))
	(else
	 (begin
	   (printf "Ignoring application ~a~%" def)
	   '()))))))

(define convert-statement
  (lambda (statement)
    (db "convert-statement: '~s'~%" statement)
    (record-case statement
       (if (test-part true-part false-part)
	   ;; FIXME: if false-part
	   (list
	    'if #\(
	    (convert-exp test-part) 
	    #\) 
	    (convert-block true-part)
	    #\ 'else
	    (convert-block (list false-part))))
       (quote (item)
	     (list (format "\"~a\"" item) #\; #\newline))
       (set! (sym exp)
	       (list
		(proper-name sym)
		#\= #\( 'object #\)
		(convert-exp exp)
		#\; #\newline))
       (let (args . bodies)
	 (convert-let args bodies))
       (begin exps
	   (convert-block exps))
       (load (filename) '())
       (else ;; apply (proc args...)
	   (list (convert-application (car statement) (cdr statement)) #\; #\newline)))))
  
(define convert-let
  (lambda (args bodies)
    (let* ((vars (map car args))
	   (temps (map (lambda (v)
			 `(object ,v #\= null #\; #\newline)) vars)))
      (append temps (convert-block bodies)))))

(define convert-exp
  (lambda (exp)
    (db "convert-exp: '~s'~%" exp)
    (if (pair? exp)
	(cond
	 ((eq? (car exp) 'quote) 
	  (format "\"~a\"" (cadr exp)))
	 (else
	  (convert-application (car exp) (cdr exp))))
	exp)))

(define convert-application
  (lambda (proc args)
    (db "convert-application: ~a(~a)~%" proc args)
    (case proc
     ((+ - * / eq? =? = set! && ||)  ;; infix
      ;; infix: handles only binary procs
      (list 
       #\(
       (convert-exp (car args))
       (proper-name proc)
       (convert-exp (cadr args))
       #\)))
     (else
      (list (proper-name proc) 
	    #\( (join-list (map convert-exp args) #\,)
	    #\))))))

(define convert-string
  (lambda (thing)
    (cond
     ((eq? thing #\;) ";")
     ((eq? thing #\newline) "\n")
     ((char? thing) (string-append (list->string (list thing)) ""))
     ((string? thing) 
      (if (and (> (string-length thing) 0)
	       (eq? (string-ref thing 0) '!))
	  (format "~a" (substr thing 1 (- (string-length thing) 1)))
	  (format "\"~a\"" thing)))
     ((symbol? thing) (format "~a" thing))
     ((list? thing) 
      (apply string-append (map convert-string thing)))
     (else (format "~s" thing)))))

(define join-strings
  (lambda (slist delim)
    (cond
     ((null? slist) "")
     ((null? (cdr slist))
      (car slist))
     (else 
      (let ((rest (join (cdr slist) delim)))
	(if (eq? rest "")
	    (car slist)
	    (string-append (car slist) delim rest)))))))

