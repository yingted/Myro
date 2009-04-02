(module srfi-8
  (receive-values receive)
  (import scheme)

;;; -*- Mode: Scheme -*-

;;;; Multiple-Value Binding Macros

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (receive-values consumer producer)
  (call-with-values producer consumer))

(define-syntax receive
  (syntax-rules ()
    ((RECEIVE (variable) producer body0 body1+ ...)
     (LET ((variable producer)) body0 body1+ ...))

    ((RECEIVE bvl producer body0 body1+ ...)
     (CALL-WITH-VALUES (LAMBDA () producer)
       (LAMBDA bvl body0 body1+ ...)))))

)
