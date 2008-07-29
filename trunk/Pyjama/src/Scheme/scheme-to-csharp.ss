(load "interpreter-ds.ss")

(define indent 0)

(define scheme-to-csharp
  (lambda (filename)
    (printf "public class Classname {~%")
    (set! indent (+ indent 1))
    (convert-list (parse-file filename))
    (set! indent (- indent 1))
    (tabs)
    (printf "}~%")))

(define tabs
  (lambda ()
    (tab indent)))

(define tab
  (lambda (n)
    (if (= n 0)
	'done
	(begin
	  (printf "   ")
	  (tab (- n 1))))))

(define convert-list
  (lambda (sexps)
    (if (null? sexps)
	'done
	(begin
	  (convert-exp (car sexps))
	  (convert-list (cdr sexps))))))

(define convert-exp
  (lambda (exp)
;;     (display exp)
;;     (newline)
    (record-case exp
      (define-exp (name body)  ;; always a lambda?
	(tabs)
	(printf "public static void ~s(" name)
	(printf "~s" (cadr body))
	(printf ") {~%")
	(set! indent (+ indent 1))
	(convert-exp (caddr body))
	(tabs)
	(printf "}~%"))
      (if-exp (test-part true-part false-part)
	(tabs)
	(printf " if (")
	(convert-exp test-part)
	(printf ") {~%")
	(tabs)
	(convert-exp true-part)
	(convert-exp false-part))
      (assign-exp (sym exp)
	(tabs)
	(convert-exp sym)
	(printf "= ")
	(convert-exp exp)
	(printf ";~%"))
      (begin-exp (exps)
	(if (null? (cdr exps))
	    (begin
	      ;;(printf "begin: ~s~%" exps)
	      (tabs)
	      (printf "return ")
	      (convert-exp (car exps))
	      (set! indent (- indent 1)))
	    (begin
	      (tabs)
	      (convert-exp (car exps))
	      (convert-exp (cons 'begin-exp (cdr exps))))))
      (app-exp (proc args)
	(if (and (variable? proc) (equal? (get-var-name proc)
					  "load"))
	    'ignore
	    (begin
	      (tabs)
	      (convert-exp proc)
	      (printf "(")
	      (convert-list args)
	      (printf ");~%"))))
      (var-exp (sym)
	       (printf " ~s " sym))
      (lit-exp (val)
	       (printf " ~s " val))
      (else (error 'convert-exp (format "~s" exp))))))

;;(scheme-to-csharp "fact-cps.ss")
