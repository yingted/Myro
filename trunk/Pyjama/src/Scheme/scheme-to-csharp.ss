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
  (lambda (filename . output)
    (call-with-input-file filename
      (lambda (port)
	(let* ((defs (read-defs port))
	       (prog (convert-program defs filename)))
	  prog)))))

(define convert-program
  (lambda (defs filename)
    (db "convert-program: '~s'~%" defs)
    (let ((name (string->symbol (car (split filename #\.)))))
      `(public class ,name #\{ #\newline
	       ,@(map convert-define defs)
	       #\} #\newline))))

;;(define convert-definitions
;;  (lambda (defs)
;;    (db "convert-definitions: '~s'~%" defs)
;;    (if (null? defs)
;;	'()
;;	(cons (convert-definition (car defs))
;;	      (convert-definitions (cdr defs))))))

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
	'if 
	(convert-exp (caar cases))  
	(convert-block (cadar cases))
	(convert-case (cdr cases))))))))

(define convert-block
  (lambda (statements)
    (db "convert-block: '~s'~%" statements)
    `(#\{ #\newline ,@(flatmap convert-statement statements) #\} #\newline)))

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
    (join-list (map (lambda (type arg)
		      (format "~a ~a" type (proper-name arg)))
		    param-types args)
	       #\,)))

(define convert-define
  (lambda (def)
    (db "convert-define: (~a)~%" def)
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
	((lambda? (caddr def))
	 ;; primitive function
	 ;; def = (define name (lambda args body ...))
	 (printf "Ignoring primitive function ~a~%" name))
	(else
	  ;; def = (define name value)
	 `(static object ,name = null))))))

(define convert-statement
  (lambda (statement)
    (db "convert-statement: '~s'~%" statement)
    (if (not (pair? statement))
	(list (convert-exp statement) #\; #\newline)
	(record-case statement
	 (define (name lamb)  ;; (define name (lambda (args) body))
	   (if (not (and (list? lamb) 
			 (eq? (car lamb) 'lambda)))
	       (begin 
		 (printf "skipping: ~a~%" statement)
		 '())
	       (let ((args (cadr lamb))
		     (body (cddr lamb)))
		 (convert-define name args body))))
	 (if (test-part true-part false-part)
	   (list
	    'if #\(
	    (convert-exp test-part) 
	    #\) #\{ #\newline
	    (convert-exp true-part)
	    (convert-exp false-part)))
	 (quote (item)
	     (list (convert-string item) #\; #\newline))
	 (set! (sym exp)
	       (list
		(proper-name sym)
		#\= 
		(convert-exp exp)
		#\; #\newline))
	 (begin (exps)
	   (convert-block exps))
	 (load (filename) '())
	 (define-datatype (filename) '())
	 (case (item case-list) ;; (case a (a 1) ...)
	   (convert-case item 
		      (caar case-list)
		      (cadar case-list)
		      (cdr case-list)))
	 (record-case (item case-list)   ;; (record-case a (a () 1) ...)
	   (convert-record-case item 
				(caar case-list)
				(cadar case-list)
				(caddar case-list)
				(cdr case-list)))
	 (cond cond-list ;; (cond (test ret) (test ret))
	   (begin
	     (db "case-list: ~s~%" cond-list)
	     (convert-cond cond-list)))
	 (else ;; apply (proc args...)
	  (cond
	   ((list? statement) 
	    (convert-application (car statement) (cdr statement)))
	   (else (list (convert-exp statement) #\;))))))))
  
(define convert-exp
  (lambda (exp)
    (db "convert-exp: '~s'~%" exp)
    (if (pair? exp)
	(cond
	 ((eq? (car exp) 'quote) (convert-string (cadr exp)))
	 (else
	  (convert-application (car exp) (cdr exp))))
	exp)))

(define convert-application
  (lambda (proc args)
    (case proc
     ((+ - * / eq? =? = set! && ||)  ;; infix
      ;; handles only binary procs
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

(define convert-case
  (lambda (item if-exp then-statements rest)
    (db "convert-case: ")
    (db "   item: ~a" item)
    (db "   if: ~a" if-exp)
    (db "   then: ~a" then-statements)
    (db "   rest: ~a" rest)
    (db "~%")
    (if (null? rest) ;; if () { body }
	(list 'if #\( (convert-exp if-exp) '== item #\) 
	      (convert-block then-statements))
	(list 'if #\( (convert-exp if-exp) '== item #\) 
	      (convert-block then-statements)
	      'else 
	      (convert-case item (caar rest) (cadar rest)
			 (cdr rest))))))

(define convert-record-case
  ;; (record-case item (m () body) ...)
  (lambda (item if-exp args then-statements rest)
    (db "convert-record-case: ")
    (db "   item: ~a" item)
    (db "   if: ~a" if-exp)
    (db "   then: ~a" then-statements)
    (db "   rest: ~a" rest)
    (db "~%")
    (if (null? rest) ;; if () { body }
	(list 'if #\( (convert-exp if-exp) '== item #\) 
	      (convert-block then-statements))
	(list 'if #\( (convert-exp if-exp) '== item #\) 
	      (convert-block then-statements)
	      'else 
	      (convert-case item (caar rest) (cadar rest)
			    (cdr rest))))))

(define convert-cond
  (lambda (cond-list)
    (if (null? cond-list)
	'()
	(let ((if-exp (caar cond-list))
	      (then-statements (cadar cond-list))
	      (rest (cdr cond-list)))
	  (db "convert-cond: ")
	  (db "   if: ~a" if-exp)
	  (db "   then: ~a" then-statements)
	  (db "   rest: ~a" rest)
	  (db "~%")
	  (let ((test-exp (convert-exp if-exp)))
	    (if (eq? test-exp 'else)
		(convert-function-def (list then-statements))		
		(if (null? rest) ;; if () { body }
		    (list 'if #\( text-exp  #\)
			  (convert-function-def (list then-statements)))
		    (list 'if #\( (convert-exp if-exp) #\)
			  (convert-function-def (list then-statements))
			  'else
			  (convert-cond rest)))))))))

(define pp
  (lambda (exp)
    (display (pp-help exp))))

(define pp-help
  (lambda (args)
    (cond
     ((null? args) "")
     (else (if (list? (car args))
	       (begin 
		 (pp-help (car args)) 
		 (pp-help (cdr args)))
	       (begin 
		 (string-append (convert-string (car args))
				" "
				(pp-help (cdr args)))))))))

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

(define convert-list
  (lambda (args)
    (cond
     ((null? args) "")
     (else
      (let ((rest (convert-list (cdr args))))
	(if (eq? rest "")
	    (list (convert-exp (car args)) rest)
	    (list (convert-exp (car args)) #\, rest)))))))

