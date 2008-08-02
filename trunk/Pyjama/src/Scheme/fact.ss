(load "transformer-macros.ss")

(define fib
  (lambda (n)
    (fib-cps n (lambda-cont (v) v))))

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

(define fact
  (lambda (n)
    (fact-cps n (lambda-cont (v) v))))

(define* fact-cps
  (lambda (n k)
    (if (= n 0)
	(k 1)
	(fact-cps (- n 1)
	  (lambda-cont (v) (k (* n v)))))))



;;	  (lambda-cont (foo)
;;	    `(,a b c ,(list one (lambda-cont (v) (k (* n v foo))))))))))

;;	  (lambda-cont (foo)
;;	    (letrec ((n 3)
;;		     (m (lambda-cont (a) (k n)))
;;		     (k 3))
;;	      (loop n (* m foo))))))))

