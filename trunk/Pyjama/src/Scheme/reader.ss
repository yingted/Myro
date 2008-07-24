;; Scanner and s-expression reader (registerized)

;; includes support for vectors, rationals, exponents, and backquote

;; scanner - character stream represented as a position number

(define chars-to-scan 'undefined)

(define 1st
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

;; scan-input takes a string and returns a list of tokens created
;; from all of the characters in the string

;; input_reg handler_reg k_reg
(define scan-input
  (lambda ()
    (set! chars-to-scan (string-append input_reg (string #\nul)))
    (set! chars_reg 0)
    (set! pc scan-input-loop)))

;; chars_reg handler_reg k_reg
(define scan-input-loop
  (lambda ()
    (set! action_reg '(goto start-state))
    (set! buffer_reg '())
    (set! k_reg (make-start-action-cont handler_reg k_reg))
    (set! pc apply-action)))

;; for testing purposes
(define test-handler (lambda (e) (list 'exception e)))

;; for testing purposes
(define test-cont '(test-cont))

;; for testing purposes
(define scan-string
  (lambda (input)
    (set! input_reg input)
    (set! handler_reg test-handler)
    (set! k_reg test-cont)
    (set! pc scan-input)
    (run)))

;; for testing purposes
(define scan-file
  (lambda (filename)
    (set! input_reg (read-content filename))
    (set! handler_reg test-handler)
    (set! k_reg test-cont)
    (set! pc scan-input)
    (run)))

;;------------------------------------------------------------------------
;; scanner actions

;; <action> ::= (shift <next-action>)
;;            | (replace <new-char> <next-action>)
;;            | (drop <next-action>)
;;            | (goto <state>)
;;            | (emit <token-type>)

;; action_reg buffer_reg chars_reg handler_reg k_reg
(define apply-action
  (lambda (action buffer chars handler k)  ;; k receives 2 args: token, chars-left
    (record-case action
      (shift (next)
	(apply-action next (cons (1st chars) buffer) (remaining chars) handler k))
      (replace (new-char next)
	(apply-action next (cons new-char buffer) (remaining chars) handler k))
      (drop (next)
	(apply-action next buffer (remaining chars) handler k))
      (goto (state)
	(apply-action (apply-state state (1st chars) handler) buffer chars handler k))
      (emit (token-type)
	(convert-buffer-to-token token-type buffer handler
	  (lambda (v) (k v chars))))
      (else (error 'apply-action "invalid action: ~a" action)))))
      
;; c_reg handler_reg
(define scan-error
  (lambda (c handler)
    (if (char=? c #\nul)
      (handler "unexpected end of input")
      (handler (format "unexpected character ~a encountered" c)))))

;; token_reg buffer_reg handler_reg k_reg
(define convert-buffer-to-token
  (lambda (token-type buffer handler k)  ;; k receives 1 token
    (let ((buffer (reverse buffer)))
      (case token-type
	(integer
	  (k (list 'integer (list->string buffer))))
	(decimal
	  (k (list 'decimal (list->string buffer))))
	(rational
	  (k (list 'rational (list->string buffer))))
	(identifier
	  (k (list 'identifier (string->symbol (list->string buffer)))))
	(boolean
	  (k (list 'boolean (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
	(character
	  (k (list 'character (car buffer))))
	(named-character
	  (let ((name (list->string buffer)))
	    (cond
	      ((string=? name "nul") (k (list 'character #\nul)))
	      ((string=? name "space") (k (list 'character #\space)))
	      ((string=? name "tab") (k (list 'character #\tab)))
	      ((string=? name "newline") (k (list 'character #\newline)))
	      ((string=? name "linefeed") (k (list 'character #\newline)))
	      ((string=? name "backspace") (k (list 'character #\backspace)))
	      ((string=? name "return") (k (list 'character #\return)))
	      ((string=? name "page") (k (list 'character #\page)))
	      (else (handler (format "invalid character name #\\~a" name))))))
	(string
	  (k (list 'string (list->string buffer))))
	(else
	  (k (list token-type)))))))

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

;; state_reg c_reg handler_reg
(define apply-state
  (lambda (state c handler)
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
	  (else (scan-error c handler))))
      (comment-state
	(cond
	  ((char=? c #\newline) '(drop (goto start-state)))
	  ((char=? c #\nul) '(goto start-state))
	  (else '(drop (goto comment-state)))))
      (comma-state
	(cond
	  ((char=? c #\@) '(drop (emit comma-at)))
	  (else '(emit comma))))
      (hash-prefix-state
	(cond
	  ((char-boolean? c) '(shift (emit boolean)))
	  ((char=? c #\\) '(drop (goto character-state)))
	  ((char=? c #\() '(drop (emit lvector)))
	  (else (scan-error c handler))))
      (character-state
	(cond
	  ((char-alphabetic? c) '(shift (goto alphabetic-character-state)))
	  ((not (char=? c #\nul)) '(shift (emit character)))
	  (else (scan-error c handler))))
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
	  ((char=? c #\nul) (scan-error c handler))
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
	  (else (scan-error c handler))))
      (identifier-state
	(cond
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  ((char-delimiter? c) '(emit identifier))
	  (else (scan-error c handler))))
      (signed-state
	(cond
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\.) '(shift (goto signed-decimal-point-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (decimal-point-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((char-delimiter? c) '(emit dot))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (signed-decimal-point-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (whole-number-state
	(cond
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\.) '(shift (goto fractional-number-state)))
	  ((char=? c #\/) '(shift (goto rational-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit integer))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (fractional-number-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit decimal))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (rational-number-state
	(cond
	  ((char-numeric? c) '(shift (goto rational-number-state*)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (rational-number-state*
	(cond
	  ((char-numeric? c) '(shift (goto rational-number-state*)))
	  ((char-delimiter? c) '(emit rational))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (suffix-state
	(cond
	  ((char-sign? c) '(shift (goto signed-exponent-state)))
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (signed-exponent-state
	(cond
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (exponent-state
	(cond
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit decimal))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else (scan-error c handler))))
      (else
	(error 'apply-state "invalid state: ~a" state)))))

;;------------------------------------------------------------------------
;; recursive descent parser
;;
;; <sexp> ::= <number> | <boolean> | <character> | <string> | <identifier>
;;          | ' <sexp>
;;          | ( <sexp>* )
;;          | ( <sexp>+ . <sexp> )
;;          | [ <sexp>* ]
;;          | [ <sexp>+ . <sexp> ]
;;          | ` <sexp>
;;          | , <sexp>
;;          | ,@ <sexp>

;; token stream represented as a list
(define first car)
(define rest-of cdr)

;; global registers
(define k_reg 'undefined)
(define tokens_reg 'undefined)
(define sexp_reg 'undefined)
(define terminator_reg 'undefined)
(define keyword_reg 'undefined)
(define pc 'undefined)

;; input_reg handler_reg k_reg
(define read-datum
  (lambda (input handler k)  ;; k receives 2 args:  sexp, tokens-left
    (scan-input input handler
      (lambda (tokens)
	(read-sexp tokens handler
	  (lambda (sexp tokens-left)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (k sexp tokens-left)
	      (handler (format "tokens left over: ~a" tokens-left)))))))))

;;(define read-datum
;;  (lambda (input)
;;    (set! tokens_reg (scan-input input))
;;    (set! k_reg (make-init-cont))
;;    (set! pc read-sexp)
;;    (run)))

;; the trampoline
(define run
  (lambda ()
    (if pc
      (begin (pc) (run))
      sexp_reg)))

;; tokens_reg k_reg
(define read-sexp
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
      (apostrophe ()
	(set! keyword_reg 'quote)
	(set! pc read-abbreviation))
      (backquote ()
	(set! keyword_reg 'quasiquote)
	(set! pc read-abbreviation))
      (comma ()
	(set! keyword_reg 'unquote)
	(set! pc read-abbreviation))
      (comma-at ()
	(set! keyword_reg 'unquote-splicing)
	(set! pc read-abbreviation))
      (lparen ()
	(set! tokens_reg (rest-of tokens_reg))
	(if (token-type? (first tokens_reg) 'dot)
	  (begin
	    (set! pc read-error))
	  (begin
	    (set! terminator_reg 'rparen)
	    (set! pc read-sexp-sequence))))
      (lbracket ()
	(set! tokens_reg (rest-of tokens_reg))
	(if (token-type? (first tokens_reg) 'dot)
	  (begin
	    (set! pc read-error))
	  (begin
	    (set! terminator_reg 'rbracket)
	    (set! pc read-sexp-sequence))))
      (lvector ()
	(set! tokens_reg (rest-of tokens_reg))
	(set! k_reg (make-vector-cont k_reg))
	(set! pc read-vector))
      (else
	(set! pc read-error)))))

;; tokens_reg keyword_reg k_reg
(define read-abbreviation
  (lambda ()
    (set! tokens_reg (rest-of tokens_reg))
    (set! k_reg (make-abbreviation-cont keyword_reg k_reg))
    (set! pc read-sexp)))

;; tokens_reg terminator_reg k_reg
(define read-sexp-sequence
  (lambda ()
    (record-case (first tokens_reg)
      ((rparen rbracket) ()
        (set! sexp_reg '())
	(set! pc close-sexp-sequence))
      (dot ()
	(set! tokens_reg (rest-of tokens_reg))
	(set! k_reg (make-dot-cont terminator_reg k_reg))
	(set! pc read-sexp))
      (else
	(set! k_reg (make-seq1-cont terminator_reg k_reg))
	(set! pc read-sexp)))))

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
	(set! pc read-error)))))

;; tokens_reg k_reg
(define read-vector
  (lambda ()
    (record-case (first tokens_reg)
      (rparen ()
	(set! sexp_reg '())
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-cont))
      (else
	(set! k_reg (make-vector-sexp1-cont k_reg))
	(set! pc read-sexp)))))

;; tokens_reg
(define read-error
  (lambda ()
    (let ((token (first tokens_reg)))
      (if (token-type? token 'end-marker)
	(error 'read "unexpected end of input")
	(error 'read "unexpected token ~a encountered" token)))))

;;------------------------------------------------------------------------
;; file reader

(define read-file
  (lambda (filename)
    (set! tokens_reg (scan-input (read-content filename)))
    (set! pc print-sexps)
    (run)))

;; tokens_reg
(define print-sexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
      (begin
	(set! sexp_reg 'done)
	(set! pc #f))      
      (begin
	(set! k_reg (make-print-sexps-cont))
	(set! pc read-sexp)))))

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

(define make-print-sexps-cont
  (lambda ()
    (list 'print-sexps-cont)))

(define make-vector-cont
  (lambda (k)
    (list 'vector-cont k)))

(define make-vector-sexp1-cont
  (lambda (k)
    (list 'vector-sexp1-cont k)))

(define make-vector-rest-cont
  (lambda (sexp1 k)
    (list 'vector-rest-cont sexp1 k)))

;; k_reg sexp_reg tokens_reg    ???? why is tokens_reg here???
(define apply-cont
  (lambda ()
    (record-case k_reg
      ;; for testing purposes
      (test-cont ()
	(set! pc #f))
      (init-cont ()
	(if (token-type? (first tokens_reg) 'end-marker)
	  (set! pc #f)
	  (error 'read "tokens left over: ~a" tokens_reg)))
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
	(set! pc read-sexp-sequence))
      (seq2-cont (sexp1 k)
	(set! k_reg k)
	(set! sexp_reg (cons sexp1 sexp_reg))
	(set! pc apply-cont))
      (print-sexps-cont ()
	(pretty-print sexp_reg)
	(set! pc print-sexps))
      (vector-cont (k)
	(set! k_reg k)
	(set! sexp_reg (list->vector sexp_reg))
	(set! pc apply-cont))
      (vector-sexp1-cont (k)
	(set! k_reg (make-vector-rest-cont sexp_reg k))
	(set! pc read-vector))
      (vector-rest-cont (sexp1 k)
	(set! k_reg k)
	(set! sexp_reg (cons sexp1 sexp_reg))
	(set! pc apply-cont))
      (scan-input-loop-cont (token k)
	(set! k_reg k)
	(set! sexp_reg (cons token sexp_reg))
	(set! pc apply-cont))
      (else (error 'apply-cont "invalid continuation: ~a" k_reg)))))

;; k_reg token_reg chars_reg
(define apply-cont2
  (lambda ()
    (record-case k_reg
      (start-action-cont (handler k)
	(if (token-type? token_reg 'end-marker)
	  (begin
	    (set! k_reg k)
	    (set! sexp_reg (list token_reg))
	    (set! pc apply-cont))
	  (begin
	    (set! handler_reg handler)
	    (set! k_reg (make-scan-input-loop-cont token_reg k))
	    (set! pc scan-input-loop))))
      (else (error 'apply-cont2 "invalid continuation: ~a" k_reg)))))

;; handler_reg exn_reg
(define apply-handler
  (lambda ()
    (record-case handler_reg
      ...)))

(define make-scan-input-loop-cont
  (lambda (token k)
    (list 'scan-input-loop-cont token k)))
