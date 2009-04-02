;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Procedures to make life easier
;;; 
;;; Copyright (c) 2008 Aaron Hsu <arcfide@sacrideo.us>
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

(module arcfide-misc
  (read-line string-split define-integrable assert-type)
  (import scheme)
  (import srfi-13)
  (import foof-loop)
  (import nested-foof-loop)

(define read-line
  (case-lambda
    [() (read-line (current-input-port))]
    [(port/eol) 
     (if (symbol? port/eol)
       (read-line (current-input-port) port/eol)
       (read-line port/eol 'lf))]
    [(port eol)
     (if (eof-object? (peek-char port))
       (eof-object)
       (collect-string ((for c (in-port port))
                        (until (end-of-line? eol c port)))
         c))]))

(define end-of-line?
  (lambda (eol c port)
    (case eol
      [(lf) (char=? c #\linefeed)]
      [(cr) (char=? c #\return)]
      [(crlf) (if (and (char=? c #\return) 
                       (char=? (peek-char port) #\linefeed))
                (begin (read-char port) #t)
                #f)]
      [else (error 'end-of-line? "Invalid EOL specifier: ~s" eol)])))

(define string-split
  (lambda (char str)
    (let ([i (string-index str char)])
      (if i
          (cons (substring str 0 i)
            (string-split char (substring str (1+ i) (string-length str))))
          (if (string-null? str) '("") (cons str '()))))))

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

(define assert-type
  (lambda (val test? type proc)
    (unless (test? val)
      (error proc "~a is not of type ~a" val type))))

)
