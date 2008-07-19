;; Scanner and s-expression reader

;; includes support for vectors, rationals, exponents, and backquote

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

(define read-datum
  (lambda (input)
    (read-sexp (scan-input input)
      (lambda (sexp tokens-left)
	(if (token-type? (first tokens-left) 'end-marker)
	  sexp
	  (error 'read "tokens left over: ~a" tokens-left))))))

(define read-sexp
  (lambda (tokens k)
    (record-case (first tokens)
      (integer (str)
	(k (string->number str) (rest-of tokens)))
      (decimal (str)
	(k (string->number str) (rest-of tokens)))
      (rational (str)
	(let ((num (string->number str)))
	  (if num
	    (k num (rest-of tokens))
	    (error 'scan "cannot represent ~a" str))))
      (boolean (bool) (k bool (rest-of tokens)))
      (character (char) (k char (rest-of tokens)))
      (string (str) (k str (rest-of tokens)))
      (identifier (id) (k id (rest-of tokens)))
      (apostrophe () (read-abbreviation tokens 'quote k))
      (backquote () (read-abbreviation tokens 'quasiquote k))
      (comma () (read-abbreviation tokens 'unquote k))
      (comma-at () (read-abbreviation tokens 'unquote-splicing k))
      (lparen ()
	(let ((tokens (rest-of tokens)))
	  (if (token-type? (first tokens) 'dot)
	    (read-error (first tokens))
	    (read-sexp-sequence tokens 'rparen k))))
      (lbracket ()
	(let ((tokens (rest-of tokens)))
	  (if (token-type? (first tokens) 'dot)
	    (read-error (first tokens))
	    (read-sexp-sequence tokens 'rbracket k))))
      (lvector ()
	(read-vector (rest-of tokens)
	  (lambda (sexps tokens-left)
	    (k (list->vector sexps) tokens-left))))
      (else (read-error (first tokens))))))

(define read-abbreviation
  (lambda (tokens keyword k)
    (read-sexp (rest-of tokens)
      (lambda (sexp tokens-left)
	(k (list keyword sexp) tokens-left)))))

(define read-sexp-sequence
  (lambda (tokens expected-terminator k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator k))
      (dot ()
	(read-sexp (rest-of tokens)
	  (lambda (sexp tokens-left)
	    (close-sexp-sequence sexp tokens-left expected-terminator k))))
      (else
	(read-sexp tokens
	  (lambda (sexp1 tokens-left)
	    (read-sexp-sequence tokens-left expected-terminator
	      (lambda (sexp2 tokens-left)
		(k (cons sexp1 sexp2) tokens-left)))))))))

(define close-sexp-sequence
  (lambda (sexp tokens expected-terminator k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (cond
	 ((token-type? (first tokens) expected-terminator)
	  (k sexp (rest-of tokens)))
	 ((eq? expected-terminator 'rparen)
	  (error 'read "parenthesized list terminated by bracket"))
	 ((eq? expected-terminator 'rbracket)
	  (error 'read "bracketed list terminated by parenthesis"))))
      (else (read-error (first tokens))))))

(define read-vector
  (lambda (tokens k)
    (record-case (first tokens)
      (rparen ()
	(k '() (rest-of tokens)))
      (else
	(read-sexp tokens
	  (lambda (sexp1 tokens-left)
	    (read-vector tokens-left
	      (lambda (sexps tokens-left)
		(k (cons sexp1 sexps) tokens-left)))))))))

(define read-error
  (lambda (token)
    (if (token-type? token 'end-marker)
      (error 'read "unexpected end of input")
      (error 'read "unexpected token ~a encountered" token))))

;;------------------------------------------------------------------------
;; file reader

(define read-file
  (lambda (filename)
    (print-sexps (scan-input (read-content filename)))))

(define print-sexps
  (lambda (tokens)
    (if (token-type? (first tokens) 'end-marker)
      'done
      (read-sexp tokens
	(lambda (sexp tokens-left)
	  (pretty-print sexp)
	  (print-sexps tokens-left))))))

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


