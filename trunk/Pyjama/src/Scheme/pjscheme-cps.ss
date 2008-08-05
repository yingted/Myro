(define *ignore-functions*
  '(read-content anything? last
    record-case-transformer let*-transformer 
    letrec-transformer let-transformer cond-transformer
    or-transformer range group search-env make-empty-environment
    search-frame apply-proc make-proc make-handler make-cont2
    raise-exp try-catch-finally-exp try-finally-exp try-catch-exp
    app-exp mu-lambda-exp lambda-exp begin-exp define-syntax-exp
    define-exp assign-exp if-exp var-exp lit-exp


    ))

(define *function-signatures*
  '(
    ))

