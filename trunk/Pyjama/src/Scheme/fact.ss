(load "transformer-macros.ss")

(define fib
  (lambda (n)
    (fib-cps n (lambda-cont (v) (halt* v)))))

(define* fib-cps
  (lambda (n k)
    (cond
      ((= n 1) (k 1))
      ((= n 2) (k 1))
      (else (fib-cps (- n 1)
	      (lambda-cont (v1)
		(fib-cps (- n 2)
		  (lambda-cont (v2)
		    (k (+ v1 v2))))))))))

(define (fact n)
  (fact-cps n (lambda-cont (v) (halt* v))))

(define* (fact-cps n k)
  (if (= n 0)
      (k 1)
      (fact-cps (- n 1)
		(lambda-cont (v) (k (* n v))))))

;; ---------------------------------------------
;; Interface for conversion to type systems:

(define *function-signatures*
  '((fib void (int))
    (fact void (int))
    (Main void ("string []"))))

(define *ignore-functions* '())

(define Main
  (lambda (args)
    (fact 5)
    (display (trampoline))
    (newline)
    (fib 15)
    (display (trampoline))
    (newline)))

;; ---------------------------------------------
