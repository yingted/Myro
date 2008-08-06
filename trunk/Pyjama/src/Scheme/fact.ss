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
  '(
    (fib void (int))
    (fact void (int))
    (fact-wrap int (int))
    (Main void ("string []"))
    ))

(define *ignore-functions* '())

;; For testing with C# conversion:

(define fact-wrap
  (lambda (n)
    (fact 5)
    (return (int) (trampoline))))

(define Main
  (lambda (args)
    (display (fact-wrap 5))
    (display (+ (string->rational "22/7")
		(string->rational "1/7")))
    (newline)
    (display (+ 5 (string->rational "1/7")))
    (newline)
    (display (+ (string->rational "1/7") 6))
    (newline)
    (display (+ 6.7 5.2))
    (newline)
    (display (+ (string->rational "1/7") 6.1))
    (newline)
    (display (+ 7.4 (string->rational "1/7")))
    (newline)
    (fact 5)
    (display (trampoline))
    (newline)
    (fib 15)
    (display (trampoline))
    (newline)))

;; ---------------------------------------------
