(define fact
  (lambda (n)
    (fact-cps n (lambda (v) v))))

;;(define blah
;;  (lambda ()
;;    (let loop ((n 3) (m (+ 2 3)))
;;      (loop n (* m m)))))

(define fact-cps
  (lambda (n k)
    (if (= n 0)
	(k 1)
	(fact-cps (- n 1) (lambda (v) v)))))

;;	(fact-cps (- n 1)
;;	  (lambda (foo)
;;	    `(,a b c ,(list one (lambda (v) (k (* n v foo))))))))))

(define fib
  (lambda (n)
    (fib-cps n (lambda (v) v))))

(define fib-cps
  (lambda (n k)
    (cond
      ((= n 1) (k 1))
      ((= n 2) (k 1))
      (else (fib-cps (- n 1)
	      (lambda (v1)
		(fib-cps (- n 2)
		  (lambda (v2)
		    (k (+ v1 v2))))))))))

