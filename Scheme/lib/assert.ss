;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assertions
;;; 
;;; Copyright (c) 2009 Aaron Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(module assertions
  ((assert enabled?) (assert-enabled? enabled?))
  (import scheme)
  
(meta define enabled? #t)

(define-syntax assert-enabled?
  (lambda (x)
    (syntax-case x ()
      [(_) enabled?]
      [(_ #f) (set! enabled? #f) (void)]
      [(_ #t) (set! enabled? #t) (void)])))

(define-syntax assert
  (lambda (x)
    (when enabled? 
      (syntax-case x (=>)
	[(_ expression => result)
	 #'(assert #f expression => result)]
	[(_ expression => proc result)
	 #'(assert #f expression => proc result)]
	[(_ proc-name expression => result)
	 #'(assert proc-name expression => equal? result)]
	[(_ proc-name expression => proc result)
	 #'(let ([expected result]
                 [actual expression]
                 [tester proc])
	     (unless (equal? expected actual)
               (error proc-name 
		 "Assertion Failure; ~s fails test (~s) against ~s"
		 actual tester expected)))]
	[(_ expression)
	 #'(assert #f expression (lambda (x) x))]
	[(_ expression tester)
	 #'(assert #f expression tester)]
	[(_ proc-name expression tester)
	 #'(let ([val expression])
             (unless (tester val)
	       (error proc-name "Assertion Failure: ~s failed (~s)"
		 val tester)))]))))

)
