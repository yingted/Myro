(define scheme-to-csharp
  (lambda (filename . output)
    (let* ((port (open-input-file filename))
	   (sexps (read-sexps port))
	   (prog (convert-statement-list sexps)))
      (if (null? output)
	  prog
	  (let ((name (string->symbol (car (split filename #\.))))
		(port (open-output-file (car output))))
	    (display (flatten prog) port))))))

(define flatten
  (lambda (l)
    (cond
     ((not (list? l)) (list l))
     ((null? l) '())
     (else (apply append (map flatten l))))))

(define read-sexps
  (lambda (port)
    (let ((sexp (read port)))
      (if (eq? sexp #!eof)
	  '()
	  (cons sexp (read-sexps port))))))

;; debug
(define db
  (lambda args
    ;; or nothing to debug off
    (apply printf args)
    'ok
    ))

(define convert-file
  (lambda (sexps filename)
    (db "convert-list: '~s'~%" sexps)
    (let ((name (string->symbol (car (split filename #\.)))))
      (list 'public 'class name #\{
	    (convert-statement-list sexps)
	    #\}))))

(define convert-statement-list
  (lambda (sexps)
    (db "convert-statement-list: '~s'~%" sexps)
    (if (null? sexps)
	'()
	(cons (convert-statement (car sexps))
	      (convert-statement-list (cdr sexps))))))

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
	#\{ 
	#\newline 
	'return 
	(convert-exp (cadar cases)) 
	#\; 
	#\newline 
	#\} 	
	#\newline )
       (convert-case (cdr cases)))))))

(define convert-statements
  (lambda (statements)
    (db "convert-statements: '~s'~%" statements)
    (#\{ (convert-statements-2 statements) #\})))

(define convert-statements-2
  (lambda (statements)
    (db "convert-statements-2: '~s'~%" statements)
    (cond
     ((null? statements) '(return null #\; #\newline))
     ((null? (cdr statements))
      (list 'return (convert-statement statements) #\; #\newline))
     (else (cons (convert-statement (car statements))
		 (convert-statements-2 (cdr statements)))))))

(define convert-statement
  (lambda (statement)
    (db "convert-statement: '~s'~%" statement)
    (if (not (pair? statement))
	(convert-exp statement)
	(record-case statement
	 (define (name lamb)  ;; (define name (lambda (args) body))
	   (if (not (and (list? lamb) 
			 (eq? (car lamb) 'lambda)))
	       (begin 
		 (printf "skipping: ~a" statement)
		 '())
	       (let ((args (cadr lamb))
		     (body (cddr lamb)))
		 (if (symbol? args)
		     (set! args (list 'varargs args))
		     (list
		      'public 'static 'void (proper-name name) #\(
		      (join-list (map (lambda (name)
					(proper-name name))
				      args)
				 #\,)
		      #\) 
		      (convert-statements body))))))
	 (if (test-part true-part false-part)
	   (list
	    'if #\(
	    (convert-exp test-part)
	    #\) #\{ #\newline
	    (convert-exp true-part)
	    (convert-exp false-part)))
	 (set! (sym exp)
	       (list
		(proper-name sym)
		#\= 
		(convert-exp exp)
		#\; #\newline))
	 (begin (exps)
	   (convert-statements exps))
	 (load (filename) '())
	 (define-datatype (filename) '())
	 (case (item case-list) ;; (case a (a 1) ...)
	   (convert-case item 
		      (caar case-list)
		      (cadar case-list)
		      (cdr case-list)))
	 (record-case (item case-list) ;; (record-case a (a () 1) ...)
	   (convert-record-case case item 
			     (caar case-list)
			     (cadar case-list)
			     (caddar case-list)
			     (cdr case-list)))
	 (cond cond-list ;; (cond (test ret) (test ret))
	   (begin
	     (db "case-list: ~s~%" cond-list)
	     (convert-cond (caar cond-list)
			(cadar cond-list)
			(cdr cond-list))))
	 (else ;; apply (proc args...)
	  (append (convert-application (car statement) (cdr statement)) 
		  (list #\;)))))))
  
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
	(list 'if #\( (convert-exp if-exp) '== item #\) #\{ 
	      (convert-statements then-statements)
	      #\})
	(list 'if #\( (convert-exp if-exp) '== item #\) #\{ 
	      (convert-statements then-statements)
	      #\} 'else 
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
	(list 'if #\( (convert-exp if-exp) '== item #\) #\{ 
	      (convert-statements then-statements)
	      #\})
	(list 'if #\( (convert-exp if-exp) '== item #\) #\{ 
	      (convert-statements then-statements)
	      #\} 'else 
	      (convert-case item (caar rest) (cadar rest)
			 (cdr rest))))))

(define convert-cond
  (lambda (if-exp then-statements rest)
    (db "convert-cond: ")
    (db "   if: ~a" if-exp)
    (db "   then: ~a" then-statements)
    (db "   rest: ~a" rest)
    (db "~%")
    (if (null? rest) ;; if () { body }
	(list 'if #\( (convert-exp if-exp) #\) #\{ 
	      (convert-statements then-statements)
	      #\})
	(list 'if #\( (convert-exp if-exp) #\) #\{ 
	      (convert-statements then-statements)
	      #\} 'else 
	      (convert-cond (caar rest) (cadar rest)
			 (cdr rest))))))

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
     ((string? thing) (format "\"~a\"" thing))
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

