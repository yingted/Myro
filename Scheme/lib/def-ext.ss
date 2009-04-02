;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extended Definition Forms
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

(module extended-definitions
  (define-integrable define-constant)
  (import scheme)

(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (identifier? #'name)
     (begin
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'((fluid-let-syntax ([name (identifier-syntax xname)])
                   (lambda formals form1 form2 ...))
                  arg
                  (... ...))])))
       (define xname
         (fluid-let-syntax ([name (identifier-syntax xname)])
           (lambda formals form1 form2 ...))))]))

(define-syntax define-constant
  (lambda (x)
    (syntax-case x ()
      [(_ name expr)
       #`(define-syntax name 
           (let ([x expr])
	     (lambda (y) 
	       (syntax-case y ()
	         [id (identifier? #'id) #`'#,x ]))))])))

)
