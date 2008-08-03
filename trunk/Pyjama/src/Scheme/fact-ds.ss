(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; continuation datatype

(define make-cont (lambda args (cons 'continuation args)))

(define*
  apply-cont
  (lambda (k value)
    (record-case (cdr k)
      (<cont-1> () (halt* value))
      (<cont-2> (v1 k) (apply-cont k (+ v1 value)))
      (<cont-3> (n k)
       (fib-cps (- n 2) (make-cont '<cont-2> value k)))
      (<cont-4> (n k) (apply-cont k (* n value)))
      (else (error 'apply-cont "bad continuation: ~a" k)))))

;;----------------------------------------------------------------------
;; main program

(define fib (lambda (n) (fib-cps n (make-cont '<cont-1>))))

(define*
  fib-cps
  (lambda (n k)
    (cond
      ((= n 1) (apply-cont k 1))
      ((= n 2) (apply-cont k 1))
      (else (fib-cps (- n 1) (make-cont '<cont-3> n k))))))

(define fact
  (lambda (n) (fact-cps n (make-cont '<cont-1>))))

(define*
  fact-cps
  (lambda (n k)
    (if (= n 0)
        (apply-cont k 1)
        (fact-cps (- n 1) (make-cont '<cont-4> n k)))))

;;(define* fake
;;  (lambda (p1 p2 p3 p4 k)
;;    (fake p4 p1 p1 p1 k)))

