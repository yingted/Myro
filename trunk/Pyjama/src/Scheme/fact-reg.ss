;;----------------------------------------------------------------------

;; global registers
(define final_reg 'undefined)
(define k_reg 'undefined)
(define n_reg 'undefined)
(define value_reg 'undefined)
(define pc 'undefined)

(define make-cont (lambda args (cons 'continuation args)))

(define apply-cont
  (lambda ()
    (record-case (cdr k_reg)
      (<cont-1> ()
       (begin (set! final_reg value_reg) (set! pc #f)))
      (<cont-2> (v1 k)
       (begin
         (set! value_reg (+ v1 value_reg))
         (set! k_reg k)
         (set! pc apply-cont)))
      (<cont-3> (n k)
       (begin
         (set! k_reg (make-cont '<cont-2> value_reg k))
         (set! n_reg (- n 2))
         (set! pc fib-cps)))
      (<cont-4> (n k)
       (begin
         (set! value_reg (* n value_reg))
         (set! k_reg k)
         (set! pc apply-cont)))
      (else (error 'apply-cont "bad continuation: ~a" k_reg)))))

(define fib
  (lambda (n)
    (begin
      (set! k_reg (make-cont '<cont-1>))
      (set! n_reg n)
      (set! pc fib-cps))))

(define fib-cps
  (lambda ()
    (cond
      ((= n_reg 1)
       (begin (set! value_reg 1) (set! pc apply-cont)))
      ((= n_reg 2)
       (begin (set! value_reg 1) (set! pc apply-cont)))
      (else
       (begin
         (set! k_reg (make-cont '<cont-3> n_reg k_reg))
         (set! n_reg (- n_reg 1))
         (set! pc fib-cps))))))

(define fact
  (lambda (n)
    (begin
      (set! k_reg (make-cont '<cont-1>))
      (set! n_reg n)
      (set! pc fact-cps))))

(define fact-cps
  (lambda ()
    (if (= n_reg 0)
        (begin (set! value_reg 1) (set! pc apply-cont))
        (begin
          (set! k_reg (make-cont '<cont-4> n_reg k_reg))
          (set! n_reg (- n_reg 1))
          (set! pc fact-cps)))))

;; the trampoline
(define trampoline
  (lambda () (if pc (begin (pc) (trampoline)) final_reg)))

(define run
  (lambda (setup . args) (apply setup args) (trampoline)))

