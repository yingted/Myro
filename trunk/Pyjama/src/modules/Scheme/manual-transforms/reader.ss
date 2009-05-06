;; Scanner and s-expression reader (registerized)

;; includes support for vectors, rationals, exponents, and backquote

;; continuations and exception handlers represented as data structures
(define make-cont list)
(define make-handler list)

;; character stream represented as a position number
(define chars-to-scan 'undefined)

(define 1st
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

;; global registers
(define input_reg 'undefined)
(define chars_reg 'undefined)
(define token_reg 'undefined)
(define tokens_reg 'undefined)
(define action_reg 'undefined)
(define buffer_reg 'undefined)
(define terminator_reg 'undefined)
(define keyword_reg 'undefined)
(define exception_reg 'undefined)
(define handler_reg 'undefined)
(define sexp_reg 'undefined)
(define k_reg 'undefined)
(define pc 'undefined)

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
    (set! k_reg (make-cont 'reader 'start-action-cont handler_reg k_reg))
    (set! pc apply-action)))

;; for testing purposes
(define test-handler (make-handler 'test-handler))

;; for testing purposes
(define test-cont (make-cont 'reader 'stop-cont))

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
  (lambda ()
    (record-case action_reg
      (shift (next)
	(set! action_reg next)
	(set! buffer_reg (cons (1st chars_reg) buffer_reg))
	(set! chars_reg (remaining chars_reg))
	(set! pc apply-action))
      (replace (new-char next)
	(set! action_reg next)
	(set! buffer_reg (cons new-char buffer_reg))
	(set! chars_reg (remaining chars_reg))
	(set! pc apply-action))
      (drop (next)
	(set! action_reg next)
	(set! chars_reg (remaining chars_reg))
	(set! pc apply-action))
      (goto (state)
	(let ((action (apply-state state (1st chars_reg))))
	  (if (eq? action 'error)
	    (begin
	      (set! pc scan-error))
	    (begin
	      (set! action_reg action)
	      (set! pc apply-action)))))
      (emit (token-type)
	(set! token_reg token-type)
	(set! k_reg (make-cont 'reader 'convert-buffer-cont chars_reg k_reg))
	(set! pc convert-buffer-to-token))
      (else (error 'apply-action "invalid action: ~a" action_reg)))))
      
;; chars_reg handler_reg
(define scan-error
  (lambda ()
    (let ((c (1st chars_reg)))
      (if (char=? c #\nul)
	(set! exception_reg "unexpected end of input")
	(set! exception_reg (format "unexpected character ~a encountered" c)))
      (set! pc apply-handler))))

;; token_reg buffer_reg handler_reg k_reg
(define convert-buffer-to-token
  (lambda ()
    (let ((buffer (reverse buffer_reg)))
      (case token_reg
	(integer
	  (set! sexp_reg (list 'integer (list->string buffer)))
	  (set! pc apply-reader-cont))
	(decimal
	  (set! sexp_reg (list 'decimal (list->string buffer)))
	  (set! pc apply-reader-cont))
	(rational
	  (set! sexp_reg (list 'rational (list->string buffer)))
	  (set! pc apply-reader-cont))
	(identifier
	  (set! sexp_reg (list 'identifier (string->symbol (list->string buffer))))
	  (set! pc apply-reader-cont))
	(boolean
	  (let ((bool (or (char=? (car buffer) #\t) (char=? (car buffer) #\T))))
	    (set! sexp_reg (list 'boolean bool))
	    (set! pc apply-reader-cont)))
	(character
	  (set! sexp_reg (list 'character (car buffer)))
	  (set! pc apply-reader-cont))
	(named-character
	  (let ((name (list->string buffer)))
	    (let ((char (cond
			  ((string=? name "nul") #\nul)
			  ((string=? name "space") #\space)
			  ((string=? name "tab") #\tab)
			  ((string=? name "newline") #\newline)
			  ((string=? name "linefeed") #\newline)
			  ((string=? name "backspace") #\backspace)
			  ((string=? name "return") #\return)
			  ((string=? name "page") #\page)
			  (else #f))))
	      (if char
		(begin
		  (set! sexp_reg (list 'character char))
		  (set! pc apply-reader-cont))
		(begin
		  (set! exception_reg (format "invalid character name #\\~a" name))
		  (set! pc apply-handler))))))
	(string
	  (set! sexp_reg (list 'string (list->string buffer)))
	  (set! pc apply-reader-cont))
	(else
	  (set! sexp_reg (list token_reg))
	  (set! pc apply-reader-cont))))))

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
	  (else 'error)))
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
(define first car)
(define rest-of cdr)

;; for testing purposes

;; read-string takes a string and parses it into the next
;; s-expression, returning the s-expression.  If there are characters
;; left over, an exception object is returned, represented as the list
;; (exception "description").

(define read-string
  (lambda (input)
    (set! input_reg input)
    (set! handler_reg test-handler)
    (set! k_reg test-cont)
    (set! pc read-datum)
    (run)))

;; input_reg handler_reg k_reg
(define read-datum
  (lambda ()
    (set! k_reg (make-cont 'reader 'read-datum-cont handler_reg k_reg))
    (set! pc scan-input)))

;; the trampoline - returns the final result in sexp_reg
(define run
  (lambda ()
    (if pc
      (begin (pc) (run))
      sexp_reg)))

;; tokens_reg handler_reg k_reg
(define read-sexp
  (lambda ()
    (record-case (first tokens_reg)
      (integer (str)
	(set! sexp_reg (string->number str))
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
      (decimal (str)
	(set! sexp_reg (string->number str))
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
      (rational (str)
	(let ((num (string->number str)))
	  (if num
	    (begin
	      (set! sexp_reg num)
	      (set! tokens_reg (rest-of tokens_reg))
	      (set! pc apply-reader-cont))
	    (begin
	      (set! exception_reg (format "cannot represent ~a" str))
	      (set! pc apply-handler)))))
      (boolean (bool)
	(set! sexp_reg bool)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
      (character (char)
	(set! sexp_reg char)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
      (string (str)
	(set! sexp_reg str)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
      (identifier (id)
	(set! sexp_reg id)
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
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
	(set! k_reg (make-cont 'reader 'vector-cont k_reg))
	(set! pc read-vector))
      (else
	(set! pc read-error)))))

;; tokens_reg keyword_reg handler_reg k_reg
(define read-abbreviation
  (lambda ()
    (set! tokens_reg (rest-of tokens_reg))
    (set! k_reg (make-cont 'reader 'abbreviation-cont keyword_reg k_reg))
    (set! pc read-sexp)))

;; tokens_reg terminator_reg handler_reg k_reg
(define read-sexp-sequence
  (lambda ()
    (record-case (first tokens_reg)
      ((rparen rbracket) ()
        (set! sexp_reg '())
	(set! pc close-sexp-sequence))
      (dot ()
	(set! tokens_reg (rest-of tokens_reg))
	(set! k_reg (make-cont 'reader 'dot-cont terminator_reg handler_reg k_reg))
	(set! pc read-sexp))
      (else
	(set! k_reg (make-cont 'reader 'seq1-cont terminator_reg handler_reg k_reg))
	(set! pc read-sexp)))))

;; sexp_reg tokens_reg terminator_reg handler_reg k_reg
(define close-sexp-sequence
  (lambda ()
    (record-case (first tokens_reg)
      ((rparen rbracket) ()
       (cond
	 ((token-type? (first tokens_reg) terminator_reg)
	  (set! tokens_reg (rest-of tokens_reg))
	  (set! pc apply-reader-cont))
	 ((eq? terminator_reg 'rparen)
	  (set! exception_reg "parenthesized list terminated by bracket")
	  (set! pc apply-handler))
	 ((eq? terminator_reg 'rbracket)
	  (set! exception_reg "bracketed list terminated by parenthesis")
	  (set! pc apply-handler))))
      (else
	(set! pc read-error)))))

;; tokens_reg handler_reg k_reg
(define read-vector
  (lambda ()
    (record-case (first tokens_reg)
      (rparen ()
	(set! sexp_reg '())
	(set! tokens_reg (rest-of tokens_reg))
	(set! pc apply-reader-cont))
      (else
	(set! k_reg (make-cont 'reader 'vector-sexp1-cont handler_reg k_reg))
	(set! pc read-sexp)))))

;; tokens_reg handler_reg
(define read-error
  (lambda ()
    (let ((token (first tokens_reg)))
      (if (token-type? token 'end-marker)
	(set! exception_reg "unexpected end of input")
	(set! exception_reg (format "unexpected token ~a encountered" token)))
      (set! pc apply-handler))))

;;------------------------------------------------------------------------
;; file reader

;; for testing purposes

;; read-file takes a filename and returns the file contents as a list
;; of tokens

(define read-file
  (lambda (filename)
    (set! input_reg (read-content filename))
    (set! handler_reg test-handler)
    (set! k_reg (make-cont 'reader 'read-file-cont test-handler))
    (set! pc scan-input)
    (run)))

;; for testing purposes

;; read-next-sexp takes a list of tokens and reads the next full sexp
;; from the tokens.  It returns the sexp and the remaining tokens as a
;; pair.
(define read-next-sexp
  (lambda (tokens)
    (set! tokens_reg tokens)
    (set! handler_reg test-handler)
    (set! k_reg (make-cont 'reader 'read-next-sexp-cont))
    (set! pc read-sexp)
    (run)))

;; for testing purposes
;; tokens_reg handler_reg
(define print-sexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
      (begin
	(set! sexp_reg 'done)
	(set! pc #f))      
      (begin
	(set! k_reg (make-cont 'reader 'print-sexps-cont handler_reg))
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

;; apply-reader-cont handles both 1-arg and 2-arg continuations, using
;; sexp_reg or sexp_reg and tokens_reg

;; k_reg sexp_reg tokens_reg
(define apply-reader-cont
  (lambda ()
    (if (not (eq? (car k_reg) 'reader))
      (apply-cont k_reg sexp_reg)
    (record-case (cdr k_reg)
      (stop-cont ()
	(set! pc #f))
      (abbreviation-cont (keyword k)
	(set! k_reg k)
	(set! sexp_reg (list keyword sexp_reg))
	(set! pc apply-reader-cont))
      (dot-cont (expected-terminator handler k)
	(set! terminator_reg expected-terminator)
	(set! handler_reg handler)
	(set! k_reg k)
	(set! pc close-sexp-sequence))
      (seq1-cont (expected-terminator handler k)
	(set! terminator_reg expected-terminator)
	(set! handler_reg handler)
	(set! k_reg (make-cont 'reader 'seq2-cont sexp_reg k))
	(set! pc read-sexp-sequence))
      (seq2-cont (sexp1 k)
	(set! k_reg k)
	(set! sexp_reg (cons sexp1 sexp_reg))
	(set! pc apply-reader-cont))
      (print-sexps-cont (handler)
	(pretty-print sexp_reg)
	(set! handler_reg handler)
	(set! pc print-sexps))
      (vector-cont (k)
	(set! k_reg k)
	(set! sexp_reg (list->vector sexp_reg))
	(set! pc apply-reader-cont))
      (vector-sexp1-cont (handler k)
	(set! k_reg (make-cont 'reader 'vector-rest-cont sexp_reg k))
	(set! handler_reg handler)
	(set! pc read-vector))
      (vector-rest-cont (sexp1 k)
	(set! k_reg k)
	(set! sexp_reg (cons sexp1 sexp_reg))
	(set! pc apply-reader-cont))
      (scan-input-loop-cont (token k)
	(set! k_reg k)
	(set! sexp_reg (cons token sexp_reg))
	(set! pc apply-reader-cont))
      (convert-buffer-cont (chars k)
	(set! k_reg k)
	(set! tokens_reg chars)
	(set! pc apply-reader-cont))
      (read-datum-cont (handler k)
	(set! tokens_reg sexp_reg)
	(set! handler_reg handler)
	(set! k_reg (make-cont 'reader 'read-sexp-cont handler k))
	(set! pc read-sexp))
      (read-file-cont (handler)
	(set! tokens_reg sexp_reg)
	(set! handler_reg handler)
	(set! pc print-sexps))
      (read-sexp-cont (handler k)
	(if (token-type? (first tokens_reg) 'end-marker)
	  (begin
	    (set! k_reg k)
	    (set! pc apply-reader-cont))
	  (begin
	    (set! handler_reg handler)
	    (set! exception_reg (format "tokens left over: ~a" tokens_reg))
	    (set! pc apply-handler))))
      (start-action-cont (handler k)
	(if (token-type? sexp_reg 'end-marker)
	  (begin
	    (set! k_reg k)
	    (set! sexp_reg (list sexp_reg))
	    (set! pc apply-reader-cont))
	  (begin
	    (set! chars_reg tokens_reg)
	    (set! handler_reg handler)
	    (set! k_reg (make-cont 'reader 'scan-input-loop-cont sexp_reg k))
	    (set! pc scan-input-loop))))
      (read-next-sexp-cont ()
	(set! sexp_reg (cons sexp_reg tokens_reg))
	(set! pc #f))
      (else (error 'apply-reader-cont "bad continuation: ~a" k_reg))))))

;; handler_reg exception_reg
(define apply-handler
  (lambda ()
    (record-case handler_reg
      (test-handler ()
	(set! sexp_reg (list 'exception exception_reg))
	(set! pc #f))
      (REP-handler ()
	(apply-cont REP-k `(uncaught exception: ,exception_reg)))
      (try-catch-handler (cvar cexps env handler k)
	;;(printf "try-handler: handling ~a exception~%" exception_reg)
	(let ((new-env (extend env (list cvar) (list exception_reg))))
	  ;;(printf "executing catch block~%")
	  ;; temporary: when we registerize the interpreter, we'll need to
	  ;; convert this function call to register form
	  (eval-sequence cexps new-env handler k)))
       (try-finally-handler (fexps env handler)
	  ;;(printf "executing finally block~%")
	  ;; temporary: when we registerize the interpreter, we'll need to
	  ;; convert this function call to register form
	  (eval-sequence fexps env handler
	    (make-cont 'interpreter 'try-finally-handler-cont handler exception_reg)))
       (try-catch-finally-handler (cvar cexps fexps env handler k)
	  ;;(printf "try-handler: handling ~a exception~%" exception_reg)
	  (let ((new-env (extend env (list cvar) (list exception_reg))))
	    (let ((catch-handler (make-handler 'try-finally-handler fexps env handler)))
	      ;;(printf "executing catch block~%")
	      ;; temporary: when we registerize the interpreter, we'll need to
	      ;; convert this function call to register form
	      (eval-sequence cexps new-env catch-handler
		(make-cont 'interpreter 'm-5 fexps env handler k)))))
      (else (error 'apply-handler "bad exception handler: ~a" handler_reg)))))
