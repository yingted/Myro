(load "transformer-macros.ss")

;; Scanner and s-expression reader

;; includes support for vectors, rationals, exponents, and backquote

;;------------------------------------------------------------------------
;; scanner - character stream represented as a position number

(define chars-to-scan 'undefined)
(define read-line-count 1)
(define read-char-count 0)

(define 1st
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

;; scan-input takes a string and returns a list of tokens created
;; from all of the characters in the string

(define* scan-input
  (lambda (input handler k)   ;; k receives a list of tokens
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 handler k)))

(define* scan-input-loop
  (lambda (chars handler k)   ;; k receives a list of tokens
    (apply-action '(goto start-state) '() chars handler
      (lambda-cont2 (token chars-left)
	(if (token-type? token 'end-marker)
	  (k (list token))
	  (scan-input-loop chars-left handler
	    (lambda-cont (tokens)
	      (k (cons token tokens)))))))))

;; for testing purposes
(define init-cont (lambda-cont (v) (halt* v)))
(define init-cont2 (lambda-cont2 (v1 v2) (halt* v1)))
(define init-handler (lambda-handler (e) (halt* (list 'exception e))))

;; for testing purposes
(define scan-string
  (lambda (input)
    (set! read-line-count 1)
    (set! read-char-count 0)
    (scan-input input init-handler init-cont)))

;; for testing purposes
(define scan-file
  (lambda (filename)
    (set! read-line-count 1)
    (set! read-char-count 0)
    (scan-input (read-content filename) init-handler init-cont)))

;;------------------------------------------------------------------------
;; scanner actions

;; <action> ::= (shift <next-action>)
;;            | (replace <new-char> <next-action>)
;;            | (drop <next-action>)
;;            | (goto <state>)
;;            | (emit <token-type>)

(define* apply-action
  (lambda (action buffer chars handler k)  ;; k receives 2 args: token, chars-left
    (record-case action
      (shift (next)
	(begin
	  (set! read-char-count (+ read-char-count 1))
	  (apply-action next (cons (1st chars) buffer) (remaining chars) handler k)))
      (replace (new-char next)
	(apply-action next (cons new-char buffer) (remaining chars) handler k))
      (drop-newline (next)
	(begin
	  (set! read-line-count (+ read-line-count 1))
	  (set! read-char-count 0)
	  (apply-action next buffer (remaining chars) handler k)))
      (drop (next)
	(begin
	  (set! read-char-count (+ read-char-count 1))
	  (apply-action next buffer (remaining chars) handler k)))
      (goto (state)
	(let ((action (apply-state state (1st chars))))
	  (if (eq? action 'error)
	    (scan-error chars handler)
	    (apply-action action buffer chars handler k))))
      (emit (token-type)
	(convert-buffer-to-token token-type buffer handler
	  (lambda-cont (v) (k (append v (list read-line-count read-char-count))
			      chars))))
      (else (error 'apply-action "invalid action: ~a" action)))))
      
(define* scan-error
  (lambda (chars handler)
    (let ((c (1st chars)))
      (if (char=? c #\nul)
	(handler 
	 (format "unexpected end of input at line: ~a col: ~a" 
		 read-line-count read-char-count))
	(handler 
	 (format "unexpected character ~a encountered at line: ~a col: ~a" 
		 c read-line-count read-char-count))))))

(define* convert-buffer-to-token
  (lambda (token-type buffer handler k)
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
	      (else (handler (format "invalid character name '~a' at line: ~a col: ~a" name read-line-count read-char-count))))))
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
	(char=? c #\')
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
	  ((char=? c #\newline) '(drop-newline (goto start-state)))
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
	  ((char=? c #\nul) '(drop (emit end-marker)))
	  (else 'error)))
      (comment-state
	(cond
	  ((char=? c #\newline) '(drop-newline (goto start-state)))
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
	  (else 'error)))
      (character-state
	(cond
	  ((char-alphabetic? c) '(shift (goto alphabetic-character-state)))
	  ((not (char=? c #\nul)) '(shift (emit character)))
	  (else 'error)))
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
	  ((char=? c #\nul) 'error)
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
	  (else 'error)))
      (identifier-state
	(cond
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  ((char-delimiter? c) '(emit identifier))
	  (else 'error)))
      (signed-state
	(cond
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\.) '(shift (goto signed-decimal-point-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (decimal-point-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((char-delimiter? c) '(emit dot))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (signed-decimal-point-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (whole-number-state
	(cond
	  ((char-numeric? c) '(shift (goto whole-number-state)))
	  ((char=? c #\.) '(shift (goto fractional-number-state)))
	  ((char=? c #\/) '(shift (goto rational-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit integer))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (fractional-number-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit decimal))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (rational-number-state
	(cond
	  ((char-numeric? c) '(shift (goto rational-number-state*)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (rational-number-state*
	(cond
	  ((char-numeric? c) '(shift (goto rational-number-state*)))
	  ((char-delimiter? c) '(emit rational))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (suffix-state
	(cond
	  ((char-sign? c) '(shift (goto signed-exponent-state)))
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (signed-exponent-state
	(cond
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit identifier))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (exponent-state
	(cond
	  ((char-numeric? c) '(shift (goto exponent-state)))
	  ((char-delimiter? c) '(emit decimal))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
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
(define first (lambda (x) (car x)))
(define rest-of (lambda (x) (cdr x)))

;; for testing purposes
(define read-string
  (lambda (input)
    (read-datum input init-handler init-cont2)))

(define* read-datum
  (lambda (input handler k)  ;; k receives 2 args:  sexp, tokens-left
    (set! read-char-count 0)
    (set! read-line-count 1)
    (scan-input input handler
      (lambda-cont (tokens)
	(read-sexp tokens handler
	  (lambda-cont2 (sexp tokens-left)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (k sexp tokens-left)
	      (handler (format "tokens left over at line: ~a col: ~a" 
			       (get-line-count (first tokens-left))
			       (get-char-count (first tokens-left))
			       )))))))))

(define get-line-count
  (lambda (token)
    (rac (rdc token))))

(define get-char-count
  (lambda (token)
    (rac token)))

(define rac
  (lambda (lyst)
    (cond
     ((null? (cdr lyst)) (car lyst))
     (else (rac (cdr lyst))))))

(define rdc
  (lambda (lyst)
    (cond
     ((null? (cdr lyst)) '())
     (else (cons (car lyst) (rdc (cdr lyst)))))))

(define string->integer
  (lambda (str)
    (string->number str)))

(define string->decimal
  (lambda (str)
    (string->number str)))

(define string->rational
  (lambda (str)
    (string->number str)))

(define true?
  (lambda (v) 
    (if v #t #f)))

(define* read-sexp
  (lambda (tokens handler k)   ;; k receives 2 args:  sexp, tokens-left
    (record-case (first tokens)
      (integer (str)
	(k (string->integer str) (rest-of tokens)))
      (decimal (str)
	(k (string->decimal str) (rest-of tokens)))
      (rational (str)
	(let ((num (string->rational str)))
	  (if (true? num)
	    (k num (rest-of tokens))
	    (handler (format "cannot represent ~a at line: ~a col: ~a" 
			     str 
			     (get-line-count (first tokens)) 
			     (get-char-count (first tokens)))))))
      (boolean (bool) (k bool (rest-of tokens)))
      (character (char) (k char (rest-of tokens)))
      (string (str) (k str (rest-of tokens)))
      (identifier (id) (k id (rest-of tokens)))
      (apostrophe () (read-abbreviation tokens 'quote handler k))
      (backquote () (read-abbreviation tokens 'quasiquote handler k))
      (comma () (read-abbreviation tokens 'unquote handler k))
      (comma-at () (read-abbreviation tokens 'unquote-splicing handler k))
      (lparen ()
	(let ((tokens (rest-of tokens)))
	  (if (token-type? (first tokens) 'dot)
	    (read-error tokens handler)
	    (read-sexp-sequence tokens 'rparen handler k))))
      (lbracket ()
	(let ((tokens (rest-of tokens)))
	  (if (token-type? (first tokens) 'dot)
	    (read-error tokens handler)
	    (read-sexp-sequence tokens 'rbracket handler k))))
      (lvector ()
	(read-vector (rest-of tokens) handler
	  (lambda-cont2 (sexps tokens-left)
	    (k (list->vector sexps) tokens-left))))
      (else (read-error tokens handler)))))

(define* read-abbreviation
  (lambda (tokens keyword handler k)  ;; k receives 2 args: sexp, tokens-left
    (read-sexp (rest-of tokens) handler
      (lambda-cont2 (sexp tokens-left)
	(k (list keyword sexp) tokens-left)))))

(define* read-sexp-sequence
  (lambda (tokens expected-terminator handler k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator handler k))
      (dot ()
	(read-sexp (rest-of tokens) handler
	  (lambda-cont2 (sexp tokens-left)
	    (close-sexp-sequence sexp tokens-left expected-terminator handler k))))
      (else
	(read-sexp tokens handler
	  (lambda-cont2 (sexp1 tokens-left)
	    (read-sexp-sequence tokens-left expected-terminator handler
	      (lambda-cont2 (sexp2 tokens-left)
		(k (cons sexp1 sexp2) tokens-left)))))))))

(define* close-sexp-sequence
  (lambda (sexp tokens expected-terminator handler k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (cond
	 ((token-type? (first tokens) expected-terminator)
	  (k sexp (rest-of tokens)))
	 ((eq? expected-terminator 'rparen)
	  (handler 
	   (format "parenthesized list terminated by bracket at line: ~a col: ~a" (get-line-count (first tokens)) (get-char-count (first tokens)))))
	 ((eq? expected-terminator 'rbracket)
	  (handler (format "bracketed list terminated by parenthesis at line: ~a col: ~a" (get-line-count (first tokens)) (get-char-count (first tokens)))))))
      (else (read-error tokens handler)))))

(define* read-vector
  (lambda (tokens handler k)
    (record-case (first tokens)
      (rparen ()
	(k '() (rest-of tokens)))
      (else
	(read-sexp tokens handler
	  (lambda-cont2 (sexp1 tokens-left)
	    (read-vector tokens-left handler
	      (lambda-cont2 (sexps tokens-left)
		(k (cons sexp1 sexps) tokens-left)))))))))

(define* read-error
  (lambda (tokens handler)
    (let ((token (first tokens)))
      (if (token-type? token 'end-marker)
	(handler (format "unexpected end of input at line: ~a col: ~a" 
			 (get-line-count token) 
			 (get-char-count token)))
	(handler (format "unexpected token ~a encountered at line: ~a col: ~a" 
			 (car token) 
			 (get-line-count token)
			 (get-char-count token)))))))

;;------------------------------------------------------------------------
;; file reader

;; for testing purposes
(define read-file
  (lambda (filename)
    (scan-input (read-content filename) init-handler
      (lambda-cont (tokens)
	(print-unparsed-sexps tokens init-handler init-cont)))))

;; for testing purposes
(define* print-unparsed-sexps
  (lambda (tokens handler k)
    (if (token-type? (first tokens) 'end-marker)
      (k 'done)
      (read-sexp tokens handler
	(lambda-cont2 (sexp tokens-left)
	  (pretty-print sexp)
	  (print-unparsed-sexps tokens-left handler k))))))

;; for testing purposes

;; read-next-sexp takes a list of tokens and reads the next full sexp
;; from the tokens.  It returns the result and the remaining tokens as
;; a pair, or an exception object of the form (exception "description")

(define read-next-sexp
  (lambda (tokens)
    (read-sexp tokens init-handler
      (lambda-cont2 (sexp tokens-left)
	(halt* (cons sexp tokens-left))))))

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

;; for testing in c#
;; (define Main
;;   (lambda (args)
;;     (let ((str (array-ref args 0)))
;;       (printf "scanning string: \\\"{0}\\\"\n" str)
;;       (scan-string str)
;;       (display (trampoline))
;;       (newline)
;;       (printf "done!\n"))))

;; Handle command-line args too
;;(define* load-files
;;  (lambda (filenames handler k)
;;    (if (null? filenames)
;;	(k 'ok)
;;	(read-datum (format "(import \\\"~a\\\")" (car filenames)) handler
;; 	    (lambda-cont2 (datum tokens-left)
;;		(printf "   (import \\\"~a\\\")...\n" (car filenames))
;; 		(parse datum handler
;; 		    (lambda-cont (exp)
;; 			(m exp toplevel-env handler
;;			   (lambda-cont (result)
;;			      (load-files (cdr filenames) handler k))))))))))

