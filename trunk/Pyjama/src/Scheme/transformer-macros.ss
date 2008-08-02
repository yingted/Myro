;; continuations

(define-syntax lambda-cont
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

(define-syntax lambda-cont2
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; procedures (closures and primitives)

(define-syntax lambda-proc
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; exception handlers

(define-syntax lambda-handler
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; definitions to registerize

(define-syntax define*
  (syntax-rules ()
    [(_ name body ...) (define name body ...)]))
