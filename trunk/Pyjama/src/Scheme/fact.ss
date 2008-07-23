;; chez scheme
(define get-time
   (lambda ()
     (let ((now (current-time)))
       (+ (time-second now)
 	 (inexact (/ (time-nanosecond now)
 		     1000000000))))))

;;;; pyjama scheme
;;(define get-time
;;  (lambda ()
;;    (current-time)))

(define fact 
  (lambda (n)
    (if (= n 1) 
	n 
	(* n (fact (- n 1))))))

(define start (get-time))
(define answer (fact 12000))
(define stop (get-time))

(display (- stop start))
(newline)
