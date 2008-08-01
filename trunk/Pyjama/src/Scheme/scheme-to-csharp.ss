(define scheme-to-csharp
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (let ((sexps (read-sexps port))
	    (name (car (split filename #\.))))
	(printf "public class ~a {~%" (proper-name name))
	(convert-list sexps)
	(printf "}~%")))))

(define read-sexps
  (lambda (port)
    (let ((sexp (read port)))
      (if (eq? sexp #!eof)
	  '()
	  (cons sexp (read-sexps port))))))

(define convert-list
  (lambda (sexps)
    (if (null? sexps)
	'done
	(begin
	  (convert-exp (car sexps))
	  (convert-list (cdr sexps))))))

(define proper-name
  (lambda (name)
    (replace name #\- #\_)))

(define replace
  (lambda (s old new)
    (list->string (sreplace (string->list s) old new '()))))

(define sreplace
  (lambda (lyst old new accum)
    (cond
     ((null? lyst) (reverse accum))
     ((eq? (car lyst) old)
      (sreplace (cdr lyst) old new
		(cons new accum)))
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

(define make-list
  (lambda (args)
    (cond
     ((null? args) "")
     (else
      (let ((rest (make-list (cdr args))))
	(if (eq? rest "")
	    (string-append (symbol->string (car args)) rest)
	    (string-append (symbol->string (car args)) ", " rest)))))))

(define print-cases
  (lambda (cases)
    (cond
     ((null? cases) "~%}~%")
     (else 
      (printf "if ~a {~%" (caar cases))
      (printf "   return ~a;~%" (cdar cases))
      (print-cases (cdr cases))))))

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
    ;;(printf "processing: '~s'~%" exp)
    (record-case exp
      (define (name body)  ;; always a lambda!
	(printf "public static void ~a(" 
		(proper-name (symbol->string name)))
	(printf (let ((args (cadr body)))
		  (if (not (list? args))
		      (proper-name (symbol->string args))
		      (join (map (lambda (name)
				   (proper-name (symbol->string name)))
				 args)
			    ", "))))
	(printf ") {~%")
	(convert-exp (caddr body))
	(printf "}~%"))
      (if (test-part true-part false-part)
	(printf " if (")
	(convert-exp test-part)
	(printf ") {~%")
	(convert-exp true-part)
	(convert-exp false-part))
      (set! (sym exp)
	(dispplay (proper-name sym))
	(printf "= ")
	(convert-exp exp)
	(printf ";~%"))
      (begin (exps)
	(if (null? (cdr exps))
	    (begin
	      ;;(printf "begin: ~s~%" exps)
	      (printf "return ")
	      (convert-exp (car exps))
	    (begin
	      (convert-exp (car exps))
	      (convert-exp (cons 'begin (cdr exps)))))))
      (load (filename) 'ignore)
      (define-datatype (filename) 'ignore)
      (cond cases
       (print-cases cases))
      (id (sym)
       (printf " ~a " (proper-name sym)))
      (else ;; apply
       (begin
	 (printf "~a();~%" exp)
	 )))))
;;       (app-exp (proc args)
;; 	(if (and (variable? proc) (equal? (get-var-name proc)
;; 					  "load"))
;; 	    'ignore
;; 	    (begin
;; 	      (convert-exp proc)
;; 	      (printf "(")
;; 	      (convert-list args)
;; 	      (printf ");~%"))))
;;       (lit-exp (val)
;; 	       (printf " ~s " val))

;;(scheme-to-csharp "fact-cps.ss")
