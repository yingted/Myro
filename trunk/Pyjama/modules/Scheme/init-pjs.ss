(define-syntax time 
  [(time ?exp) (let ((start (current-time)))
		 ?exp
		 (printf "~s seconds ellapsed\n" (- (current-time) start)))])
