(define scheme-to-csharp
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (let ((sexps (read-sexps port))
	    (name (car (split filename #\.))))
	(string-append
	 (format "public class ~a {~%" (proper-name name))
	 (convert-list sexps)
	 (format "}~%"))))))

(define read-sexps
  (lambda (port)
    (let ((sexp (read port)))
      (if (eq? sexp #!eof)
	  '()
	  (cons sexp (read-sexps port))))))

(define convert-list
  (lambda (sexps)
    (if (null? sexps)
	""
	(string-append (convert-exp (car sexps))
		       (convert-list (cdr sexps))))))

(define proper-name
  (lambda (name)
    (replace name #\- #\_)))

(define replace
  (lambda (s old new)
    (list->string 
     (sreplace 
      (string->list 
       (make-string s)) old new '()))))

(define sreplace
  (lambda (lyst old new accum)
    (cond
     ((null? lyst) (reverse accum))
     ((eq? (car lyst) old)
      (sreplace (cdr lyst) old new
		(cons new accum)))
     (else (sreplace (cdr lyst) old new
		     (cons (car lyst) accum))))))

(define make-string
  (lambda (thing)
    (cond
     ((string? thing) thing)
     ((symbol? thing) (format "~a" thing))
     ((list? thing) (make-list thing))
     (else (format "~s" thing)))))

(define split
  (lambda (s delim)
    (let ((list (string->list (make-string s))))
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

(define make-list
  (lambda (args)
    (cond
     ((null? args) "")
     (else
      (let ((rest (make-list (cdr args))))
	(if (eq? rest "")
	    (string-append (make-string 
			    (convert-exp (car args))) rest)
	    (string-append (make-string 
			    (convert-exp (car args))) ", " rest)))))))

(define get-cases
  (lambda (cases)
    (cond
     ((null? cases) (format "~%}~%"))
     (else 
      (string-append
       (format "if ~a {~%" (convert-exp (caar cases)))
       (format "   return ~a;~%" (cdar cases))
       (get-cases (cdr cases)))))))

(define join
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

(define convert-exp
  (lambda (exp)
    (printf "processing: '~s'~%" exp)
    (if (pair? exp)
    (record-case exp
      (define (name body)  ;; always a lambda!
	(string-append
	 (format "public static void ~a(" 
		 (proper-name (make-string name)))
	 (format (let ((args (cadr body)))
		   (if (not (list? args))
		       (proper-name (make-string args))
		       (join (map (lambda (name)
				    (proper-name (make-string name)))
				  args)
			     ", "))))
	 (format ") {~%")
	 (convert-exp (caddr body))
	 (format "}~%")))
      (if (test-part true-part false-part)
	(string-append
	 (format " if (")
	 (convert-exp test-part)
	 (format ") {~%")
	 (convert-exp true-part)
	 (convert-exp false-part)))
      (set! (sym exp)
	(string-append
	 (proper-name sym)
	 (format "= ")
	 (convert-exp exp)
	 (format ";~%")))
      (begin (exps)
	(if (null? (cdr exps))
	    (string-append
	     "return "
	     (convert-exp (car exps)))
	    (string-append
	     (convert-exp (car exps))
	     (convert-exp (cons 'begin (cdr exps))))))
      (load (filename) "")
      (define-datatype (filename) "")
      (cond cases
       (get-cases cases))
      (quote (item)
       (format "\"~a\"" (convert-exp item)))
      (id (sym)
       (format " ~a " (proper-name sym)))
      (else ;; apply (proc args...)
       (let ((proc (proper-name (car exp))))
	 (format "~a(~a)" proc (make-list (cdr exp))))))
    (make-string exp))))
