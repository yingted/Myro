;; --

(define fact 
  (lambda (n)
    (if (zero? n) 
	1
	(* n (fact (- n 1))))))

;; --

(define fact-cps
  (lambda (n k)
    (if (zero? n) 
	(k 1)
	(fact-cps (sub1 n) 
	   (lambda (v) 
              (k (* n v)))))))

;; --

(define fact-ds
  (lambda (n k)
    (if (zero? n)
	(apply-cont k 1)
	(fact-ds (sub1 n)
	   (make-cont 'id00001 n k)))))

(define make-cont list)

(define apply-cont 
  (lambda (k value)
    (record-case k
       (identity () 
	  value)
       (id00001 (n k)
	  (apply-cont k (* n value)))
       (odd-ds? (k)
	  (apply-cont k value))
       (even-ds? (k)
	  (apply-cont k value))
       (fib-1 (v1 k)
	  (apply-cont k (+ v1 value)))
       (fib-2 (n k)
	  (fib-ds (- n 2) (make-cont 'fib-1 value k)))
       )))

;; -- 

;; n k
(define fact-reg
  (lambda ()
    (if (zero? n) 
	(begin
	  (set! value 1)
	  (set! pc apply-cont-reg))
	(begin
	  (set! k (make-cont 'id00001 n k))
	  (set! n (sub1 n))
	  (set! pc fact-reg)))))

(define start
  (lambda ()
    (set! n 20)
    (set! k (make-cont 'identity))
    (set! pc fib-reg)
    (run)))

(define run
  (lambda ()
    (if pc
	(begin 
	  (pc) 
	  (run))
	retval)))

;; k value
(define apply-cont-reg
  (lambda ()
    (record-case k
       (identity () 
	  (set! retval value)
	  (set! pc #f))
       (id00001 (n_ k_)
	  (set! k k_)
	  (set! value (* n_ value))
	  (set! pc apply-cont-reg))
       (odd-reg? (k_)
	  (set! k k_)
	  (set! n value)
	  (set! pc apply-cont-reg))
       (even-reg? (k_)
	  (set! k k_)
	  (set! n value)
	  (set! pc apply-cont-reg))
       (fib-1 (v_ k_)
	  (set! k k_)
	  (set! value (+ v_ value))
	  (set! pc apply-cont-reg))
       (fib-2 (n_ k_)
	  (set! k (make-cont 'fib-1 value k_))
	  (set! n (- n_ 2))
	  (set! pc fib-reg))
       )))
  
;; ---

(define odd?
  (lambda (n)
    (if (= n 1)
	#t
	(even? (- n 1)))))

(define even?
  (lambda (n)
    (if (= n 1)
	#f
	(odd? (- n 1)))))

;; ---

(define odd-cps?
  (lambda (n k)
    (if (= n 1)
	(k #t)
	(even-cps? (- n 1) 
	     (lambda (v)
	       (k v))))))

(define even-cps?
  (lambda (n k)
    (if (= n 1)
	(k #f)
	(odd-cps? (- n 1) 
	     (lambda (v)
	       (k v))))))

;; ---

;; n k
(define odd-reg?
  (lambda ()
    (if (= n 1)
	(begin
	  ;;(k #t)
	  (set! value #t)
	  (set! pc apply-cont-reg))
	(begin
	  (set! n (- n 1))
	  (set! k (make-cont 'odd-reg? k))
	  (set! pc even-reg?)))))

;; n k
(define even-reg?
  (lambda ()
    (if (= n 1)
	(begin
	  ;;(k #f)
	  (set! value #f)
	  (set! pc apply-cont-reg))
	(begin
	  (set! n (- n 1))
	  (set! k (make-cont 'even-reg? k))
	  (set! pc odd-reg?)))))

;; ---

(define odd-ds?
  (lambda (n k)
    (if (= n 1)
	(apply-cont k #t)
	(even-ds? (- n 1)
	    (make-cont 'odd-ds? k)))))

(define even-ds?
  (lambda (n k)
    (if (= n 1)
	(apply-cont k #f)
	(odd-ds? (- n 1)
	    (make-cont 'even-ds? k)))))

;; ---

(define fib
  (lambda (n)
    (cond
     ((= n 1) 1)
     ((= n 2) 1)
     (else (+ (fib (- n 1))
	      (fib (- n 2)))))))

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
  
(define fib-ds
  (lambda (n k)
    (cond
     ((= n 1) (apply-cont k 1))     
     ((= n 2) (apply-cont k 1))
     (else (fib-ds (- n 1) (make-cont 'fib-2 n k))))))

(define fib-reg
  (lambda ()
    (cond
     ((= n 1) 
      (begin
	(set! pc apply-cont-reg)
	(set! k k)
	(set! value 1)))
     ((= n 2) 
      (begin
	(set! pc apply-cont-reg)
	(set! k k)
	(set! value 1)))
     (else 
      (begin
	(set! k (make-cont 'fib-2 n k))
	(set! pc fib-reg)
	(set! n (- n 1)))))))
