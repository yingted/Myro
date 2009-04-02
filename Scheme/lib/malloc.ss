;;; These procedures are taken from the Chez Scheme User's Guide 7
;;; Make sure you have the libc.so shared object or equivalent loaded.

(module gc-malloc 
  (malloc malloc-region? malloc-region-ptr register-region)
  (import scheme)
  
(define-record malloc-region (ptr))

(define $malloc
  (foreign-procedure "malloc" (uptr) uptr))

(define $free
  (foreign-procedure "free" (uptr) void))
  
(define do-malloc
  (lambda (size)
    (make-malloc-region ($malloc size))))

(define do-free
  (lambda (region)
    ($free (malloc-region-ptr region))))
    
(define malloc-guardian (make-guardian))

(define malloc
  (lambda (size)
    (let ([x (do-malloc size)])
      (malloc-guardian x)
      x)))
      
(define register-region
  (lambda (pointer)
    (let ([x (make-malloc-region pointer)])
      (malloc-guardian x)
      x)))

(collect-request-handler
  (lambda ()
    (collect)
    (let f ()
      (let ((x (malloc-guardian)))
        (when x
          (do-free x)
          (f))))))

)
