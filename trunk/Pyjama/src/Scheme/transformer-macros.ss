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

;; where the computation should halt

(define-syntax halt*
  (syntax-rules ()
    [(_ value) value]))

;; for languages that need a return statement

(define-syntax return*
  (syntax-rules ()
    [(_ value) value]
    [(_ type value) value]))

;; marks all expressions that are quoted in registerized code

(define-syntax quote*
  (syntax-rules ()
    [(_ original string) (quote original)]))
