;; (define time 
;;   (lambda ()
;;     (let ((now (current-time)))
;;       (+ (time-second now)
;; 	 (inexact (/ (time-nanosecond now)
;; 		     1000000000))))))

(define start (time))

(define fact 
  (lambda (n)
    (if (= n 1) 
	n 
	(* n (fact (- n 1))))))

(write (fact 12000))
(newline)
(write (- (time) start))
(newline)


(try (fact n)
     (except (e) (fact (- n 1)))
     (except (e) (error "error")))

