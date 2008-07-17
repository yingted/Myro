;; includes support for vectors, rationals, exponents, and backquote

;; data structure representations of actions and states

;;------------------------------------------------------------------------
;; scanner - character stream represented as a list

;;(define 1st car)
;;(define remaining cdr)

;;(define scan-input
;;  (lambda (input)
;;    (let scan ((chars (append (string->list input) (list #\nul)))
;;	       (tokens '()))
;;      (let ((answer (apply-action '(goto start-state) '() chars)))
;;	(let ((token (car answer))
;;	      (chars-left (cdr answer)))
;;	  (if (token-type? token 'end-marker)
;;	    (reverse (cons token tokens))
;;	    (scan chars-left (cons token tokens))))))))

;;------------------------------------------------------------------------
;; scanner - character stream represented as a position number

(define chars-to-scan 'undefined)

(define 1st
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

(define scan-input
  (lambda (input)
    (set! chars-to-scan (string-append input (string #\nul)))
    (let scan ((chars 0) (tokens '()))
      (let ((answer (apply-action '(goto start-state) '() chars)))
	(let ((token (car answer))
	      (chars-left (cdr answer)))
	  (if (token-type? token 'end-marker)
	    (reverse (cons token tokens))
	    (scan chars-left (cons token tokens))))))))

;;------------------------------------------------------------------------
;; scanner actions

;; <action> ::= (shift <next-action>)
;;            | (replace <new-char> <next-action>)
;;            | (drop <next-action>)
;;            | (goto <state>)
;;            | (emit <token-type>)

(define apply-action
  (lambda (action buffer chars)
    (record-case action
      (shift (next)
	(apply-action next (cons (1st chars) buffer) (remaining chars)))
      (replace (new-char next)
	(apply-action next (cons new-char buffer) (remaining chars)))
      (drop (next)
	(apply-action next buffer (remaining chars)))
      (goto (state)
	(apply-action (apply-state state (1st chars)) buffer chars))
      (emit (token-type)
	(make-answer (convert-buffer-to-token token-type buffer) chars))
      (else (error 'apply-action "invalid action: ~a" action)))))
      
(define scan-error
  (lambda (c)
    (if (char=? c #\nul)
      (error 'scan "unexpected end of input")
      (error 'scan "unexpected character ~a encountered" c))))

(define make-answer cons)

(define convert-buffer-to-token
  (lambda (token-type buffer)
    (let ((buffer (reverse buffer)))
      (case token-type
	(integer
	  (list 'integer (list->string buffer)))
	(decimal
	  (list 'decimal (list->string buffer)))
	(rational
	  (list 'rational (list->string buffer)))
	(identifier
	  (list 'identifier (string->symbol (list->string buffer))))
	(boolean
	  (list 'boolean (or (char=? (car buffer) #\t) (char=? (car buffer) #\T))))
	(character
	  (list 'character (car buffer)))
	(named-character
	  (let ((name (list->string buffer)))
	    (cond
	      ((string=? name "nul") (list 'character #\nul))
	      ((string=? name "space") (list 'character #\space))
	      ((string=? name "tab") (list 'character #\tab))
	      ((string=? name "newline") (list 'character #\newline))
	      ((string=? name "linefeed") (list 'character #\newline))
	      ((string=? name "backspace") (list 'character #\backspace))
	      ((string=? name "return") (list 'character #\return))
	      ((string=? name "page") (list 'character #\page))
	      (else (error 'scan "invalid character name #\\~a" name)))))
	(string
	  (list 'string (list->string buffer)))
	(else
	  (list token-type))))))

(define token-type?
  (lambda (token class)
    (eq? (car token) class)))

;;------------------------------------------------------------------------
;; character categories

(define char-delimiter?
  (lambda (c)
    (or (char-whitespace? c)
	(char=? c #\()
	(char=? c #\[)
	(char=? c #\))
	(char=? c #\])
	(char=? c #\")
	(char=? c #\;)
	(char=? c #\#)
	(char=? c #\nul))))

(define char-initial?
  (lambda (c)
    (or (char-alphabetic? c)
	(char=? c #\!)
	(char=? c #\$)
	(char=? c #\%)
	(char=? c #\&)
	(char=? c #\*)
	(char=? c #\/)
	(char=? c #\:)
	(char=? c #\<)
	(char=? c #\=)
	(char=? c #\>)
	(char=? c #\?)
	(char=? c #\^)
	(char=? c #\_)
	(char=? c #\~))))

(define char-special-subsequent?
  (lambda (c)
    (or (char=? c #\+)
	(char=? c #\-)
	(char=? c #\@)
	(char=? c #\.))))

(define char-subsequent?
  (lambda (c)
    (or (char-initial? c)
	(char-numeric? c)
	(char-special-subsequent? c))))

(define char-sign?
  (lambda (c)
    (or (char=? c #\+)
	(char=? c #\-))))

(define char-boolean?
  (lambda (c)
    (or (char=? c #\t)
	(char=? c #\T)
	(char=? c #\f)
	(char=? c #\F))))

;;------------------------------------------------------------------------
;; finite-state automaton

(define apply-state
  (lambda (state c)
    (case state
      (start-state
	(cond
	  ((char-whitespace? c) '(drop (goto start-state)))
	  ((char=? c #\;) '(drop (goto comment-state)))
	  ((char=? c #\() '(drop (emit lparen)))
	  ((char=? c #\[) '(drop (emit lbracket)))
	  ((char=? c #\)) '(drop (emit rparen)))
	  ((char=? c #\]) '(drop (emit rbracket)))
	  ((char=? c #\') '(drop (emit apostrophe)))
	  ((char=? c #\`) '(drop (emit backquote)))
	  ((char=? c #\,) '(drop (goto comma-state)))
	  ((char=? c #\#) '(drop (goto hash-prefix-state)))
	  ((char=? c #\") '(drop (goto string-state)))
	  ((char-initial? c) '(shift (goto identifier-state)))
	  ((char-sign? c) '(shift (goto signed-state)))
	  ((char=? c #\.) '(shift (goto decimal-point-state)))
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\nul) '(emit end-marker))
	  (else (scan-error c))))
      (comment-state
	(cond
	  ((char=? c #\newline) '(drop (goto start-state)))
	  ((char=? c #\nul) '(goto start-state))
	  (else '(drop (goto comment-state)))))
      ;; new
      (comma-state
	(cond
	  ((char=? c #\@) '(drop (emit comma-at)))
	  (else '(emit comma))))
      (hash-prefix-state
	(cond
	  ((char-boolean? c) '(shift (emit boolean)))
	  ((char=? c #\\) '(drop (goto character-state)))
	  ((char=? c #\() '(drop (emit lvector)))
	  (else (scan-error c))))
      (character-state
	(cond
	  ((char-alphabetic? c) '(shift (goto alphabetic-character-state)))
	  ((not (char=? c #\nul)) '(shift (emit character)))
	  (else (scan-error c))))
      (alphabetic-character-state
	(cond
	  ((char-alphabetic? c) '(shift (goto named-character-state)))
	  (else '(emit character))))
      (named-character-state
	(cond
	  ((char-delimiter? c) '(emit named-character))
	  (else '(shift (goto named-character-state)))))
      (string-state
	(cond
	  ((char=? c #\") '(drop (emit string)))
	  ((char=? c #\\) '(drop (goto string-escape-state)))
	  ((char=? c #\nul) (scan-error c))
	  (else '(shift (goto string-state)))))
      (string-escape-state
	(cond
	  ((char=? c #\") '(shift (goto string-state)))
	  ((char=? c #\\) '(shift (goto string-state)))
	  ((char=? c #\b) '(replace #\backspace (goto string-state)))
	  ((char=? c #\f) '(replace #\page (goto string-state)))
	  ((char=? c #\n) '(replace #\newline (goto string-state)))
	  ((char=? c #\t) '(replace #\tab (goto string-state)))
	  ((char=? c #\r) '(replace #\return (goto string-state)))
	  (else (scan-error c))))
      (identifier-state
	(cond
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  ((char-delimiter? c) '(emit identifier))
	  (else (scan-error c))))
      (signed-state
	(cond
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\.) '(shift (goto signed-decimal-point-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (decimal-point-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((char-delimiter? c) '(emit dot))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (signed-decimal-point-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (whole-number-state
	(cond
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\.) '(shift (goto fractional-number-state)))
	  ((char=? c #\/) '(shift (goto rational-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit integer))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (fractional-number-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit decimal))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (rational-number-state
	(cond
	  ((char-numeric? c) '(shift (goto rational-number-state*)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (rational-number-state*
	(cond
	  ((char-numeric? c) '(shift (goto rational-number-state*)))
	  ((char-delimiter? c) '(emit rational))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (suffix-state
	(cond
	  ((char-sign? c) '(shift (goto signed-exponent-state)))
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (signed-exponent-state
	(cond
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (exponent-state
	(cond
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit decimal))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c))))
      (else
	(error 'apply-state "invalid state: ~a" state)))))

;;------------------------------------------------------------------------
;; parser (registerized)

;; token stream represented as a list
(define first car)
(define rest-of cdr)

;; global registers
(define tokens_reg 'undefined)
(define k_reg 'undefined)
(define terminator_reg 'undefined)
(define sexp_reg 'undefined)
(define pc 'undefined)
;; new
(define keyword_reg 'undefined)

(define parse
  (lambda (input)
    (set! tokens_reg (scan-input input))
    (set! k_reg (make-init-cont))
    (set! pc parse-sexp)
    (run)))

;; the trampoline
(define run
  (lambda ()
    (if pc
      (begin (pc) (run))
      sexp_reg)))

;; tokens_reg k_reg
(define parse-sexp
  (lambda ()
    (record-case (first tokens_reg)
      (integer (str)
	(set! sexp_reg (string->number str))
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (decimal (str)
	(set! sexp_reg (string->number str))
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (rational (str)
	(let ((num (string->number str)))
	  (if num
	    (begin
	      (set! sexp_reg num)
	      (set! tokens_reg (rest-of tokens_reg))
	      (set! pc apply-cont))
	    (error 'scan "cannot represent ~a" str))))
      (boolean (bool)
	(set! sexp_reg bool)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (character (char)
	(set! sexp_reg char)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (string (str)
	(set! sexp_reg str)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (identifier (id)
	(set! sexp_reg id)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      ;; new
      (apostrophe ()
	(set! keyword_reg 'quote)
	(set! pc parse-abbreviation))
      ;; new
      (backquote ()
	(set! keyword_reg 'quasiquote)
	(set! pc parse-abbreviation))
      ;; new
      (comma ()
	(set! keyword_reg 'unquote)
	(set! pc parse-abbreviation))
      ;; new
      (comma-at ()
	(set! keyword_reg 'unquote-splicing)
	(set! pc parse-abbreviation))
      (lparen ()
	(set! tokens_reg (rest-of tokens_reg))
	(if (token-type? (first tokens_reg) 'dot)
	  (begin
	    (set! pc parse-error))
	  (begin
	    (set! terminator_reg 'rparen)
	    (set! pc parse-sexp-sequence))))
      (lbracket ()
	(set! tokens_reg (rest-of tokens_reg))
	(if (token-type? (first tokens_reg) 'dot)
	  (begin
	    (set! pc parse-error))
	  (begin
	    (set! terminator_reg 'rbracket)
	    (set! pc parse-sexp-sequence))))
      (lvector ()
	(set! tokens_reg (rest-of tokens_reg))
	(set! k_reg (make-vector-cont k_reg))
	(set! pc parse-vector))
      (else
	(set! pc parse-error)))))

;; new
;; tokens_reg keyword_reg k_reg
(define parse-abbreviation
  (lambda ()
    (set! tokens_reg (rest-of tokens_reg))
    (set! k_reg (make-abbreviation-cont keyword_reg k_reg))
    (set! pc parse-sexp)))

;; tokens_reg terminator_reg k_reg
(define parse-sexp-sequence
  (lambda ()
    (record-case (first tokens_reg)
      ((rparen rbracket) ()
        (set! sexp_reg '())
	(set! pc close-sexp-sequence))
      (dot ()
	(set! tokens_reg (rest-of tokens_reg))
	(set! k_reg (make-dot-cont terminator_reg k_reg))
	(set! pc parse-sexp))
      (else
	(set! k_reg (make-seq1-cont terminator_reg k_reg))
	(set! pc parse-sexp)))))

;; sexp_reg tokens_reg terminator_reg k_reg
(define close-sexp-sequence
  (lambda ()
    (record-case (first tokens_reg)
      ((rparen rbracket) ()
       (cond
	 ((token-type? (first tokens_reg) terminator_reg)
	  (set! tokens_reg (rest-of tokens_reg))
	  (set! pc apply-cont))
	 ((eq? terminator_reg 'rparen)
	  (error 'read "parenthesized list terminated by bracket"))
	 ((eq? terminator_reg 'rbracket)
	  (error 'read "bracketed list terminated by parenthesis"))))
      (else
	(set! pc parse-error)))))

;; tokens_reg k_reg
(define parse-vector
  (lambda ()
    (record-case (first tokens_reg)
      (rparen ()
	(set! sexp_reg '())
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (else
	(set! k_reg (make-vector-sexp1-cont k_reg))
	(set! pc parse-sexp)))))

;; tokens_reg
(define parse-error
  (lambda ()
    (let ((token (first tokens_reg)))
      (if (token-type? token 'end-marker)
	(error 'read "unexpected end of input")
	(error 'read "unexpected token ~a encountered" token)))))

;;------------------------------------------------------------------------
;; file loader

(define load-file
  (lambda (filename)
    (set! tokens_reg (scan-input (read-content filename)))
    (set! pc process-sexps)
    (run)))

;; tokens_reg
(define process-sexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
      (begin
	(set! sexp_reg 'done)
	(set! pc #f))      
      (begin
	(set! k_reg (make-process-cont))
	(set! pc parse-sexp)))))

;; returns the entire file contents as a single string
(define read-content
  (lambda (filename)
    (apply string
      (call-with-input-file filename
	(lambda (port)
	  (let loop ((char (read-char port)))
	    (if (eof-object? char)
	      '()
	      (cons char (loop (read-char port))))))))))

;;------------------------------------------------------------------------
;; continuations

(define make-init-cont
  (lambda ()
    (list 'init-cont)))

;; new
(define make-abbreviation-cont
  (lambda (keyword k)
    (list 'abbreviation-cont keyword k)))

(define make-dot-cont
  (lambda (expected-terminator k)
    (list 'dot-cont expected-terminator k)))

(define make-seq1-cont
  (lambda (expected-terminator k)
    (list 'seq1-cont expected-terminator k)))

(define make-seq2-cont
  (lambda (sexp1 k)
    (list 'seq2-cont sexp1 k)))

(define make-process-cont
  (lambda ()
    (list 'process-cont)))

(define make-vector-cont
  (lambda (k)
    (list 'vector-cont k)))

(define make-vector-sexp1-cont
  (lambda (k)
    (list 'vector-sexp1-cont k)))

(define make-vector-rest-cont
  (lambda (sexp1 k)
    (list 'vector-rest-cont sexp1 k)))

;; k_reg sexp_reg tokens_reg
(define apply-cont
  (lambda ()
    (record-case k_reg
      (init-cont ()
	(if (token-type? (first tokens_reg) 'end-marker)
	  (set! pc #f)
	  (error 'read "tokens left over: ~a" tokens_reg)))
      ;; new
      (abbreviation-cont (keyword k)
	(set! k_reg k)
	(set! sexp_reg (list keyword sexp_reg))
	(set! pc apply-cont))
      (dot-cont (expected-terminator k)
	(set! terminator_reg expected-terminator)
	(set! k_reg k)
	(set! pc close-sexp-sequence))
      (seq1-cont (expected-terminator k)
	(set! terminator_reg expected-terminator)
	(set! k_reg (make-seq2-cont sexp_reg k))
	(set! pc parse-sexp-sequence))
      (seq2-cont (sexp1 k)
	(set! k_reg k)
	(set! sexp_reg (cons sexp1 sexp_reg))
	(set! pc apply-cont))
      (process-cont ()
	(pretty-print sexp_reg)
	(set! pc process-sexps))
      (vector-cont (k)
	(set! k_reg k)
	(set! sexp_reg (list->vector sexp_reg))
	(set! pc apply-cont))
      (vector-sexp1-cont (k)
	(set! k_reg (make-vector-rest-cont sexp_reg k))
	(set! pc parse-vector))
      (vector-rest-cont (sexp1 k)
	(set! k_reg k)
	(set! sexp_reg (cons sexp1 sexp_reg))
	(set! pc apply-cont))
      (else (error 'apply-cont "invalid continuation: ~a" k_reg)))))

