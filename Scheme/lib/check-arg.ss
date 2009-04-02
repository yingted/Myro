;; Simple CHECK-ARG procedure for argument checking.

(module check-arg
  (check-arg)
  (import scheme)

(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error 'check-arg "Bad argument: ~s ~s ~s"
                             val pred caller)))))

)
