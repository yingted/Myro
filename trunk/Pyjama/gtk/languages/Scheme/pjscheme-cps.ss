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

(load "transformer-macros.ss")

;; Environments represented as data structures

;; bindings

(define make-binding
  (lambda (variable value)
    (list variable "" value)))

(define binding-variable
  (lambda (binding)
    (car binding)))

(define binding-docstring
  (lambda (binding)
    (cadr binding)))

(define binding-value
  (lambda (binding)
    (caddr binding)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-car! (cdr binding) docstring)))

(define set-binding-value!
  (lambda (binding value)
    (set-car! (cddr binding) value)))

;; frames

(define make-frame
  (lambda (variables values)
    (map make-binding variables values)))

(define first-binding
  (lambda (frame)
    (car frame)))

(define rest-of-bindings
  (lambda (frame)
    (cdr frame)))

(define empty-frame?
  (lambda (frame)
    (null? frame)))

(define search-frame
  (lambda (frame variable)
    (cond
      ((empty-frame? frame) #f)
      ((eq? (binding-variable (first-binding frame)) variable)
       (first-binding frame))
      (else (search-frame (rest-of-bindings frame) variable)))))

;; environments

;; <environment> = (environment . (<frame> ...))

(define environment?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda ()
    (cons 'environment '(()))))

(define make-initial-environment
  (lambda (vars vals)
    (cons 'environment (list (make-frame vars vals)))))

(define first-frame
  (lambda (env)
    (cadr env)))

(define frames
  (lambda (env)
    (cdr env)))

(define set-first-frame!
  (lambda (env new-frame)
    (set-car! (cdr env) new-frame)))

(define extend
  (lambda (env variables values)
    (cons 'environment (cons (make-frame variables values) (cdr env)))))

;; variable lookup

(define search-env
  (lambda (env variable)
    (search-frames (cdr env) variable)))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
      #f
      (let ((binding (search-frame (car frames) variable)))
        (if binding
          binding
          (search-frames (cdr frames) variable))))))

(define* lookup-value
  (lambda (variable env handler k)
    (lookup-binding variable env handler
      (lambda-cont (binding)
	(k (binding-value binding))))))

(define* lookup-binding
  (lambda (variable env handler k)
    (let ((binding (search-env env variable)))
      (if binding
	(k binding)
	(split-variable variable
	  (lambda-cont (components)
            (if (dlr-env-contains variable)
                (k (dlr-env-lookup variable))
                (if components
		    (lookup-variable-components components "" env handler k)
                    (handler (format "unbound variable ~a" variable))))))))))

(define dlr-env-contains
  (lambda (variable)
    #t))

(define dlr-env-lookup
  (lambda (variable)
    (binding 42)))

;; adds a new binding for var to the first frame if one doesn't exist
(define* lookup-binding-in-first-frame
  (lambda (var env handler k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (k binding)
          (let ((new-binding (make-binding var 'undefined)))
            (let ((new-frame (cons new-binding frame)))
              (set-first-frame! env new-frame)
	      (k new-binding))))))))

(define* lookup-variable-components
  ;; math.x.y.z
  ;; components: '(test x y z) "" ...
  ;; components: '(x y z) "test" ...
  ;; components: '(y z) "test.x" ...
  ;; components: '(z) "test.x.z" ...
  (lambda (components path env handler k)
    ;;(printf "components: ~s path: ~s\n" components path)
    (let ((var (car components)))
      (lookup-module-binding var env path handler
	(lambda-cont (binding)
	  (if (null? (cdr components))
	    (k binding)
	    (let ((result (binding-value binding))
		  (new-path (if (string=? path "")
			      (format "~a" var)
			      (format "~a.~a" path var))))
	      (if (not (environment? result))
		  (handler (format "~a is not a module" new-path))
		  (lookup-variable-components
		    (cdr components) new-path result handler k)))))))))

(define* lookup-module-binding
  (lambda (var env path handler k)
    (let ((binding (search-env env var)))
      (cond
	(binding (k binding))
	((string=? path "") (handler (format "unbound variable ~a" var)))
	(else (handler (format "unbound variable ~a in module ~a" var path)))))))

(define* split-variable
  (lambda (variable k)
    (let ((strings (group (string->list (symbol->string variable)) #\.)))
      (if (or (member "" strings) (= (length strings) 1))
	(k #f)
	(k (map string->symbol strings))))))

(define group
  (lambda (chars delimiter)
    (letrec
      ((position
	(lambda (chars)
	  (if (char=? (car chars) delimiter)
	      0
	      (+ 1 (position (cdr chars))))))
       (group
	 (lambda (chars)
	   (cond
	     ((null? chars) '())
	     ((not (member delimiter chars)) (list (apply string chars)))
	     (else (let ((n (position chars)))
		     (cons (apply string (list-head chars n))
			   (group (cdr (list-tail chars n))))))))))
      (group chars))))
(load "transformer-macros.ss")

;;--------------------------------------------------------------------------
;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader-cps.ss")
(load "unifier-cps.ss")

;; The core grammar
;;
;; <exp> ::= <literal>
;;         | (quote <datum>)
;;         | (quasiquote <datum>)
;;         | <var>
;;         | (if <exp> <exp> <exp>)
;;         | (set! <var> <exp>)
;;         | (define <var> <exp>)
;;         | (define-syntax <keyword> (<pattern> <pattern>) ...)
;;         | (begin <exp> ...)
;;         | (lambda (<formal> ...) <exp> ...)
;;         | (lambda <formal> <exp> ...)
;;         | (<exp> <exp> ...)
;;         | (try <body> (catch <var> <exp> ...))
;;         | (try <body> (finally <exp> ...))
;;         | (try <body> (catch <var> <exp> ...) (finally <exp> ...))
;;         | (raise <exp>)
;;         | (dict (<exp> <exp>) ...)

(define-datatype expression expression?
  (lit-exp
   (datum anything?))
  (var-exp
    (id symbol?))
  (if-exp
   (test-exp expression?)
   (then-exp expression?)
   (else-exp expression?))
  (assign-exp
    (var symbol?)
    (rhs-exp expression?))
  (define-exp
    (id symbol?)
    (rhs-exp (list-of expression?)))
  (define-syntax-exp
    (keyword symbol?)
    (clauses (list-of (list-of pattern?))))
  (begin-exp
    (exps (list-of expression?)))
  (lambda-exp
    (formals (list-of symbol?))
    (body expression?))
  (mu-lambda-exp
    (formals (list-of symbol?))
    (runt symbol?)
    (body expression?))
  (app-exp
    (operator expression?)
    (operands (list-of expression?)))
  (try-catch-exp
    (body expression?)
    (catch-var symbol?)
    (catch-exps (list-of expression?)))
  (try-finally-exp
    (body expression?)
    (finally-exps (list-of expression?)))
  (try-catch-finally-exp
    (body expression?)
    (catch-var symbol?)
    (catch-exps (list-of expression?))
    (finally-exps (list-of expression?)))
  (raise-exp
    (exp expression?))
  (dict-exp
    (pairs (list-of (list-of expression?))))
  )

;;--------------------------------------------------------------------------
;; Macro support

(load "environments-cps.ss")

(define syntactic-sugar?
  (lambda (datum)
    (and (pair? datum)
	 (symbol? (car datum))
	 (true? (search-env macro-env (car datum))))))

(define make-pattern-macro
  (lambda (clauses)
    (cons 'pattern-macro clauses)))

(define macro-clauses
  (lambda (macro)
    (cdr macro)))

(define pattern-macro?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'pattern-macro))))

(define* expand-once
  (lambda (datum handler k)
    (lookup-value (car datum) macro-env handler
      (lambda-cont (macro)
	(if (pattern-macro? macro)
	  (process-macro-clauses (macro-clauses macro) datum handler k)
	  (macro datum k))))))

(define* process-macro-clauses
  (lambda (clauses datum handler k)
    (if (null? clauses)
      (handler (format "no matching clause found for ~a" datum))
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses)))
	(unify-patterns left-pattern datum
	  (lambda-cont (subst)
	    (if subst
	      (instantiate right-pattern subst k)
	      (process-macro-clauses (cdr clauses) datum handler k))))))))

(define mit-define-transformer
  (lambda-macro (datum k) 
    (let ((name (caadr datum))
	  (formals (cdadr datum))
	  (bodies (cddr datum)))
      (k `(define ,name (lambda ,formals ,@bodies))))))

(define and-transformer
  (lambda-macro (datum k)
    (let ((exps (cdr datum)))
      (cond
	((null? exps) (k '#t))
	((null? (cdr exps)) (k (car exps)))
	(else (k `(if ,(car exps) (and ,@(cdr exps)) #f)))))))

;; avoids variable capture
(define or-transformer
  (lambda-macro (datum k)
    (let ((exps (cdr datum)))
      (cond
	((null? exps) (k '#f))
	((null? (cdr exps)) (k (car exps)))
	(else (k `(let ((bool ,(car exps))
			(else-code (lambda () (or ,@(cdr exps)))))
		    (if bool bool (else-code)))))))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer
  (lambda-macro (datum k)
    (let ((clauses (cdr datum)))
      (if (null? clauses)
	(error 'cond-transformer "bad concrete syntax: ~a" datum)
	(let ((first-clause (car clauses))
	      (other-clauses (cdr clauses)))
	  (if (or (null? first-clause) (not (list? first-clause)))
	    (error 'cond-transformer "bad concrete syntax: ~a" datum)
	    (let ((test-exp (car first-clause))
		  (then-exps (cdr first-clause)))
	      (cond
		((eq? test-exp 'else)
		 (cond
		   ((null? then-exps) (error 'cond-transformer "bad concrete syntax: (~a)" 'else))
		   ((null? (cdr then-exps)) (k (car then-exps)))
		   (else (k `(begin ,@then-exps)))))
		((null? then-exps)
		 (if (null? other-clauses)
		   (k `(let ((bool ,test-exp))
			 (if bool bool)))
		   (k `(let ((bool ,test-exp)
			     (else-code (lambda () (cond ,@other-clauses))))
			 (if bool bool (else-code))))))
		((null? other-clauses)
		 (if (null? (cdr then-exps))
		   (k `(if ,test-exp ,(car then-exps)))
		   (k `(if ,test-exp (begin ,@then-exps)))))
		((null? (cdr then-exps))
		 (k `(if ,test-exp ,(car then-exps) (cond ,@other-clauses))))
		(else (k `(if ,test-exp (begin ,@then-exps) (cond ,@other-clauses))))))))))))

(define let-transformer
  (lambda-macro (datum k)
    (if (symbol? (cadr datum))
      ;; named let
      (let* ((name (cadr datum))
	     (bindings (caddr datum))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cdddr datum)))
	(k `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
      ;; ordinary let
      (let* ((bindings (cadr datum))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cddr datum)))
	(k `((lambda ,vars ,@bodies) ,@exps))))))

(define letrec-transformer
  (lambda-macro (datum k)
    (let* ((decls (cadr datum))
	   (vars (map car decls))
	   (procs (map cadr decls))
	   (bodies (cddr datum)))
      (create-letrec-assignments vars procs
	(lambda-cont2 (bindings assigns)
	  (k `(let ,bindings ,@assigns ,@bodies)))))))

(define* create-letrec-assignments
  (lambda (vars procs k2)
    (if (null? vars)
      (k2 '() '())
      (create-letrec-assignments (cdr vars) (cdr procs)
	(lambda-cont2 (bindings assigns)
	  (k2 (cons `(,(car vars) 'undefined) bindings)
	      (cons `(set! ,(car vars) ,(car procs)) assigns)))))))

(define let*-transformer
  (lambda-macro (datum k)
    (let ((bindings (cadr datum))
	  (bodies (cddr datum)))
      (nest-let*-bindings bindings bodies k))))

(define* nest-let*-bindings
  (lambda (bindings bodies k)
    (if (or (null? bindings)
	    (null? (cdr bindings)))
	(k `(let ,bindings ,@bodies))
	(nest-let*-bindings (cdr bindings) bodies
	  (lambda-cont (v)
	    (k `(let (,(car bindings)) ,v)))))))

;; avoids variable capture
(define case-transformer
  (lambda-macro (datum k)
    (let ((exp (cadr datum))
	  (clauses (cddr datum)))
      ;; if exp is a variable, no need to introduce r binding
      (if (symbol? exp)
	(case-clauses->simple-cond-clauses exp clauses
	  (lambda-cont (new-clauses)
	    (k `(cond ,@new-clauses))))
	(case-clauses->cond-clauses 'r clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses)))))))))

(define* case-clauses->simple-cond-clauses
  (lambda (var clauses k)
    (if (null? clauses)
      (k '())
      (case-clauses->simple-cond-clauses var (cdr clauses)
	(lambda-cont (new-clauses)
	  (let ((clause (car clauses)))
	    (cond
	      ((eq? (car clause) 'else)
	       (k (cons clause new-clauses)))
	      ((symbol? (car clause))
	       (k (cons `((eq? ,var ',(car clause)) ,@(cdr clause)) new-clauses)))
	      (else (k (cons `((memq ,var ',(car clause)) ,@(cdr clause))
			     new-clauses))))))))))

(define* case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (case-clauses->cond-clauses var (cdr clauses)
	(lambda-cont2 (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq? (car clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr clause))) bindings)
		  (cons '(else (else-code)) new-clauses))
	      (if (symbol? (car clause))
		(let ((name (car clause)))
		  (k2 (cons `(,name (lambda () ,@(cdr clause))) bindings)
		      (cons `((eq? ,var ',(car clause)) (,name)) new-clauses)))
		(let ((name (caar clause)))
		  (k2 (cons `(,name (lambda () ,@(cdr clause))) bindings)
		      (cons `((memq ,var ',(car clause)) (,name)) new-clauses)))))))))))

;; avoids variable capture
(define record-case-transformer
  (lambda-macro (datum k)
    (let ((exp (cadr datum))
	  (clauses (cddr datum)))
      ;; if exp is a variable, no need to introduce r binding
      (if (symbol? exp)
	(record-case-clauses->cond-clauses exp clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ,bindings (cond ,@new-clauses)))))
	(record-case-clauses->cond-clauses 'r clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses)))))))))

(define* record-case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses var (cdr clauses)
	(lambda-cont2 (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq? (car clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr clause))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol? (car clause))
		(let ((name (car clause)))
		  (k2 (cons `(,name (lambda ,(cadr clause) ,@(cddr clause))) bindings)
		      (cons `((eq? (car ,var) ',(car clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (caar clause)))
		  (k2 (cons `(,name (lambda ,(cadr clause) ,@(cddr clause))) bindings)
		      (cons `((memq (car ,var) ',(car clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))

(define make-macro-env
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case)
      (list and-transformer
	    or-transformer
	    cond-transformer
	    let-transformer
	    letrec-transformer
	    let*-transformer
	    case-transformer
	    record-case-transformer))))

(define macro-env (make-macro-env))

;;--------------------------------------------------------------------------

;; for testing purposes
(define parse-string
  (lambda (string)
    (read-datum string init-handler
      (lambda-cont2 (datum tokens-left)
	(parse datum init-handler init-cont)))))

(define* parse
  (lambda (datum handler k)
    (cond
      ((literal? datum) (k (lit-exp datum)))
      ((quote? datum) (k (lit-exp (cadr datum))))
      ((quasiquote? datum)
       (expand-quasiquote (cadr datum) handler
	 (lambda-cont (v)
	   (parse v handler k))))
      ((unquote? datum) (handler (format "misplaced ~a" datum)))
      ((unquote-splicing? datum) (handler (format "misplaced ~a" datum)))
      ((symbol? datum) (k (var-exp datum)))
      ((syntactic-sugar? datum)
       (expand-once datum handler
	 (lambda-cont (v)
	   (parse v handler k))))
      ((if-then? datum)
       (parse (cadr datum) handler
	 (lambda-cont (v1)
	   (parse (caddr datum) handler
	     (lambda-cont (v2)
	       (k (if-exp v1 v2 (lit-exp #f))))))))
      ((if-else? datum)
       (parse (cadr datum) handler
	 (lambda-cont (v1)
	   (parse (caddr datum) handler
	     (lambda-cont (v2)
	       (parse (cadddr datum) handler
		 (lambda-cont (v3)
		   (k (if-exp v1 v2 v3)))))))))
      ((assignment? datum)
       (parse (caddr datum) handler
	 (lambda-cont (v)
	   (k (assign-exp (cadr datum) v)))))
      ((define? datum)
       (if (mit-style? datum)
	 (mit-define-transformer datum
	   (lambda-cont (v)
	     (parse v handler k)))
	 (if (= (length datum) 3) ;; (define x 1)
	     (parse (caddr datum) handler 
		(lambda-cont (body)
		    (k (define-exp (cadr datum) (list body)))))
	     (parse (cadddr datum) handler ;; (define x "" 8)
		 (lambda-cont (body)
		    (parse (caddr datum) handler
			(lambda-cont (docstring)
			    (k (define-exp (cadr datum) (list docstring body))))))))))
      ((define-syntax? datum)
       (k (define-syntax-exp (cadr datum) (cddr datum))))
      ((begin? datum)
       (parse-all (cdr datum) handler
	 (lambda-cont (v)
	   (cond
	     ((null? v) (handler (format "bad concrete syntax: ~a" datum)))
	     ((null? (cdr v)) (k (car v)))
	     (else (k (begin-exp v)))))))
      ((lambda? datum)
       (parse (cons 'begin (cddr datum)) handler
	 (lambda-cont (body)
	   (if (list? (cadr datum))
	     (k (lambda-exp (cadr datum) body))
	     (k (mu-lambda-exp (head (cadr datum)) (last (cadr datum)) body))))))
      ((try? datum)
       (cond
	 ((= (length datum) 2)
	  ;; (try <body>)
	  (parse (try-body datum) handler k))
	 ((and (= (length datum) 3) (catch? (caddr datum)))
	  ;; (try <body> (catch <var> <exp> ...))
	  (parse (try-body datum) handler
	    (lambda-cont (body)
	      (parse-all (catch-exps (caddr datum)) handler
		(lambda-cont (cexps)
		  (let ((cvar (catch-var (caddr datum))))
		    (k (try-catch-exp body cvar cexps))))))))
	 ((and (= (length datum) 3) (finally? (caddr datum)))
	  ;; (try <body> (finally <exp> ...))
	  (parse (try-body datum) handler
	    (lambda-cont (body)
	      (parse-all (finally-exps (caddr datum)) handler
		(lambda-cont (fexps)
		  (k (try-finally-exp body fexps)))))))
	 ((and (= (length datum) 4) (catch? (caddr datum)) (finally? (cadddr datum)))
	  ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	  (parse (try-body datum) handler
	    (lambda-cont (body)
	      (parse-all (catch-exps (caddr datum)) handler
		(lambda-cont (cexps)
		  (parse-all (finally-exps (cadddr datum)) handler
		    (lambda-cont (fexps)
		      (let ((cvar (catch-var (caddr datum))))
			(k (try-catch-finally-exp body cvar cexps fexps))))))))))
	 (else (handler (format "bad try syntax: ~a" datum)))))
      ((raise? datum)
       (parse (cadr datum) handler
	 (lambda-cont (v)
	   (k (raise-exp v)))))
      ((dict? datum)
       (parse-pairs (cdr datum) handler
	  (lambda-cont (v1)
	     (k (dict-exp v1)))))
      ((application? datum)
       (parse (car datum) handler
	 (lambda-cont (v1)
	   (parse-all (cdr datum) handler
	     (lambda-cont (v2)
	       (k (app-exp v1 v2)))))))
      (else (handler (format "bad concrete syntax: ~a" datum))))))

(define* parse-pairs
  (lambda (pairs handler k)
    (if (null? pairs)
      (k '())
      (parse (caar pairs) handler
	(lambda-cont (a)
	  (parse (cadar pairs) handler
	    (lambda-cont (b)
              (parse-pairs (cdr pairs) handler
                  (lambda-cont (results)
		      (k (cons (list a b) results)))))))))))

(define* parse-all
  (lambda (datum-list handler k)
    (if (null? datum-list)
      (k '())
      (parse (car datum-list) handler
	(lambda-cont (a)
	  (parse-all (cdr datum-list) handler
	    (lambda-cont (b)
	      (k (cons a b)))))))))

(define* expand-quasiquote
  (lambda (datum handler k)
    (cond
      ((vector? datum)
       (expand-quasiquote (vector->list datum) handler
	 (lambda-cont (ls) (k `(list->vector ,ls)))))
      ((not (pair? datum)) (k `(quote ,datum)))
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) (k `(quote ,datum)))
      ((unquote? datum) (k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
	 (k (cadr (car datum)))
	 (expand-quasiquote (cdr datum) handler
	   (lambda-cont (v) (k `(append ,(cadr (car datum)) ,v))))))
      ((quasiquote-list? datum)
       (expand-quasiquote-list datum handler
	 (lambda-cont (v)
	   (k `(list ,@v)))))
      (else
	(expand-quasiquote (car datum) handler
	  (lambda-cont (v1)
	    (expand-quasiquote (cdr datum) handler
	      (lambda-cont (v2)
		(k `(cons ,v1 ,v2))))))))))

(define* expand-quasiquote-list
  (lambda (datum handler k)
    (if (null? datum)
      (k '())
      (expand-quasiquote (car datum) handler
	(lambda-cont (v1)
	  (expand-quasiquote-list (cdr datum) handler
	    (lambda-cont (v2)
	       (k (cons v1 v2)))))))))

(define quasiquote-list?
  (lambda (datum)
    (or (null? datum)
	(and (pair? datum)
	     ;; doesn't handle nested quasiquotes yet
	     (not (quasiquote? datum))
	     (not (unquote? datum))
	     (not (unquote-splicing? datum))
	     ;; doesn't handle nested quasiquotes yet
	     (not (quasiquote? (car datum)))
	     (not (unquote-splicing? (car datum)))
	     (quasiquote-list? (cdr datum))))))

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((pair? (cdr formals)) (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define mit-style?
  (lambda (datum)
    (not (symbol? (cadr datum)))))

(define literal?
  (lambda (datum)
    (or (number? datum)
	(boolean? datum)
	(char? datum)
	(string? datum)
	(vector? datum))))

(define anything?
  (lambda (datum) #t))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (and (list? datum)
	   (op (length datum) len)
	   (eq? (car datum) tag)))))

(define quote? (tagged-list 'quote = 2))
(define quasiquote? (tagged-list 'quasiquote = 2))
(define unquote? (tagged-list 'unquote = 2))
(define unquote-splicing? (tagged-list 'unquote-splicing = 2))
(define if-then? (tagged-list 'if = 3))
(define if-else? (tagged-list 'if = 4))
(define assignment? (tagged-list 'set! = 3))
(define define? (tagged-list 'define >= 3))
(define define-syntax? (tagged-list 'define-syntax >= 3))
(define begin? (tagged-list 'begin >= 2))
(define lambda? (tagged-list 'lambda >= 3))
(define raise? (tagged-list 'raise = 2))
(define dict? (tagged-list 'dict >= 1))

(define application?
  (lambda (datum)
    (and (list? datum)
	 (not (null? datum))
	 (not (reserved-keyword? (car datum))))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
	 (memq x '(quote quasiquote lambda if set! define begin
		    cond and or let let* letrec case record-case
		    try catch finally raise dict
		    )))))

(define try? (tagged-list 'try >= 2))
(define try-body (lambda (x) (cadr x)))
(define catch? (tagged-list 'catch >= 3))
(define catch-var (lambda (x) (cadr x)))
(define catch-exps (lambda (x) (cddr x)))
(define finally? (tagged-list 'finally >= 2))
(define finally-exps (lambda (x) (cdr x)))

;;------------------------------------------------------------------------
;; file parser

;; for testing purposes
(define print-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (get-parsed-sexps filename))))

;; for testing purposes
(define get-parsed-sexps
  (lambda (filename)
    (scan-input (read-content filename) init-handler 
      (lambda-cont (tokens) 
	(parse-sexps tokens init-handler init-cont)))))

;; for testing purposes
(define* parse-sexps
  (lambda (tokens handler k)
    (if (token-type? (first tokens) 'end-marker)
      (k '())
      (read-sexp tokens handler
	(lambda-cont2 (datum tokens-left)
	  (parse datum handler
	    (lambda-cont (exp)
	      (parse-sexps tokens-left handler
		(lambda-cont (v)
		  (k (cons exp v)))))))))))
(load "transformer-macros.ss")

;;----------------------------------------------------------------------------
;; Interpreter

(load "environments-cps.ss")
(load "parser-cps.ss")

;; temporary
(define testall
  (lambda ()
    (read-datum "(load \"examples.ss\")" REP-handler
      (lambda-cont2 (datum tokens-left)
	(parse datum REP-handler
	  (lambda-cont (exp)
	    (m exp toplevel-env REP-handler
	      (lambda-cont (v)
		(read-datum "(test-all)" REP-handler
		  (lambda-cont2 (datum tokens-left)
		    (parse datum REP-handler
		      (lambda-cont (exp)
			(m exp toplevel-env REP-handler
			  (lambda-cont (v)
			    (read-datum "(exit)" REP-handler
			      (lambda-cont2 (datum tokens-left)
				(parse datum REP-handler
				  (lambda-cont (exp)
				    (m exp toplevel-env REP-handler REP-k)))))))))))))))))))

(define start
  (lambda ()
    (read-eval-print)))

(define *need-newline* #f)

(define REP-k
  (lambda-cont (v)
    (if (not (eq? v '<void>))
	(pretty-print-prim v))
    (if *need-newline* (newline))
    (read-eval-print)))

(define pretty-print-prim
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print arg)))

(define newline-prim
  (lambda ()
    (set! *need-newline* #f)
    (newline)))

(define display-prim
  (lambda (arg)
    (let* ((s (format "~s" arg))
	   (len (string-length s)))
      (set! *need-newline* (true? (not (equal? (substring s (- len 1) len) "\n"))))
      (display s))))

(define REP-handler
  (lambda-handler (e)
    (REP-k `(uncaught exception: ,e))))

(define read-line
  (lambda (prompt)
    (printf prompt)
    (read)))

(define* read-eval-print
  (lambda ()
    (let* ((input (read-line "==> "))
	   (input-string (format "~s" input)))
      (read-datum input-string REP-handler
	(lambda-cont2 (datum tokens-left)
	  (parse datum REP-handler
	    (lambda-cont (exp)
	      (m exp toplevel-env REP-handler REP-k))))))))

(define* m
  (lambda (exp env handler k)
    (cases expression exp
      (lit-exp (datum) (k datum))
      (var-exp (id) (lookup-value id env handler k))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler
	  (lambda-cont (bool)
	    (if bool
	      (m then-exp env handler k)
	      (m else-exp env handler k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler
	  (lambda-cont (rhs-value)
	    (lookup-binding var env handler
	      (lambda-cont (binding)
		(set-binding-value! binding rhs-value)
		(k '<void>))))))
      (define-exp (var rhs-exp)
        (if (= (length rhs-exp) 1)
            (m (car rhs-exp) env handler
              (lambda-cont (rhs-value)
                (lookup-binding-in-first-frame var env handler
                  (lambda-cont (binding)
                    (set-binding-value! binding rhs-value)
                    (k '<void>)))))
            (m (cadr rhs-exp) env handler ;; body
              (lambda-cont (rhs-value)
		 (m (car rhs-exp) env handler ;; docstring
		    (lambda-cont (docstring)
		       (lookup-binding-in-first-frame var env handler
			  (lambda-cont (binding)
			     (set-binding-docstring! binding docstring)
			     (set-binding-value! binding rhs-value)
			     (k '<void>)))))))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler
	  (lambda-cont (binding)
	    (set-binding-value! binding (make-pattern-macro clauses))
	    (k '<void>))))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp (formals body)
	(k (closure formals body env)))
      (mu-lambda-exp (formals runt body)
	(k (mu-closure formals runt body env)))
      (try-catch-exp (body cvar cexps)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler k)))
      (try-finally-exp (body fexps)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler
	    (lambda-cont (v)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler
		(lambda-cont (v2) (k v)))))))
      (try-catch-finally-exp (body cvar cexps fexps)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler
	     (lambda-cont (v)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler
		 (lambda-cont (v2) (k v)))))))
      (raise-exp (exp)
	(m exp env handler
	  ;; todo: pass in more info to handler (k, env)
	  (lambda-cont (e) (handler e))))
      (dict-exp (pairs)
	(k (list 'dict pairs)))
      (app-exp (operator operands)
	 (m* operands env handler
            (lambda-cont (args)
	       (m operator env handler
	           (lambda-cont (proc)
		      (if (dlr-exp? proc)
			  (k (dlr-apply proc args))
			  (proc args env handler k)))))))
      (else (error 'm "bad abstract syntax: ~a" exp)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (lambda-handler (e)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e))))
	;;(printf "executing catch block~%")
	(eval-sequence cexps new-env handler k)))))

(define try-finally-handler
  (lambda (fexps env handler)
    (lambda-handler (e)
      ;;(printf "executing finally block~%")
      (eval-sequence fexps env handler
	(lambda-cont (v)
	  ;;(printf "propagating ~a exception~%" e)
	  (handler e))))))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (lambda-handler (e)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e))))
	(let ((catch-handler (try-finally-handler fexps env handler)))
	  ;;(printf "executing catch block~%")
	  (eval-sequence cexps new-env catch-handler
	    (lambda-cont (v)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler
		(lambda-cont (v2) (k v))))))))))

(define closure
  (lambda (formals body env)
    (lambda-proc (args env2 handler k2)
      (if (= (length args) (length formals))
	(m body (extend env formals args) handler k2)
	(handler "incorrect number of arguments")))))

(define mu-closure
  (lambda (formals runt body env)
    (lambda-proc (args env2 handler k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (m body new-env handler k2))
	(handler "not enough arguments given")))))

(define* m*
  (lambda (exps env handler k)
    (if (null? exps)
      (k '())
      (m (car exps) env handler
	(lambda-cont (v1)
	  (m* (cdr exps) env handler
	    (lambda-cont (v2)
	      (k (cons v1 v2)))))))))

(define* eval-sequence
  (lambda (exps env handler k)
    (m (car exps) env handler
       (lambda-cont (result)
	 (if (null? (cdr exps))
	   (k result)
	   (eval-sequence (cdr exps) env handler k))))))

(define make-initial-env-extended
  (lambda (env)
    ;; this is here as a hook for extending environments in C# etc.
    env))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
     (make-initial-environment
      (list 'exit 'eval 'parse 'parse-string 'apply 'sqrt 'print 'display 'newline 'load 'null? 'cons 'car 'cdr
	    'list '+ '- '* '/ '< '> '= 'equal? 'eq? 'memq 'range 'set-car! 'set-cdr!
	    'import 'get 'call-with-current-continuation 'call/cc
	    'reverse 'append 'list->vector 'dir 'current-time 'map 'for-each 'env
	    'using 'not 'printf 'vector 'vector-set! 'vector-ref 'make-vector 'help)
      (list
	;; exit
        (lambda-proc (args env2 handler k2)
	  (set! macro-env (make-macro-env))
	  (set! toplevel-env (make-toplevel-env))
	  ;; temporary
	  (set! load-stack '())
	  (halt* '(exiting the interpreter)))
	;; eval
	(lambda-proc (args env2 handler k2)
	  (parse (car args) handler
	    (lambda-cont (exp)
	      (m exp toplevel-env handler k2))))   ;; use toplevel-env here?
	;; parse
	(lambda-proc (args env2 handler k2)
	  (parse (car args) handler k2))
	;; parse-string
	(lambda-proc (args env2 handler k2)
	  (read-datum (car args) handler
	    (lambda-cont2 (datum tokens-left)
	      (parse datum handler k2))))
	;; apply
	(lambda-proc (args env2 handler k2)
	  (let ((proc (car args))
		(proc-args (cadr args)))
	    (proc proc-args env2 handler k2)))
	;; sqrt
	(lambda-proc (args env2 handler k2) (k2 (apply sqrt args)))
	;; print
	(lambda-proc (args env2 handler k2) (for-each pretty-print-prim args) (k2 '<void>))
	;; display
	(lambda-proc (args env2 handler k2) (apply display-prim args) (k2 '<void>))
	;; newline
	(lambda-proc (args env2 handler k2) (newline-prim) (k2 '<void>))
	;; load
	(lambda-proc (args env2 handler k2) 
	   (set! load-stack '())
	   (load-file (car args) toplevel-env handler k2))
	;; null?
	(lambda-proc (args env2 handler k2) (k2 (apply null? args)))
	;; cons
	(lambda-proc (args env2 handler k2) (k2 (apply cons args)))
	;; car
	(lambda-proc (args env2 handler k2) (k2 (apply car args)))
	;; cdr
	(lambda-proc (args env2 handler k2) (k2 (apply cdr args)))
	;; list
	(lambda-proc (args env2 handler k2) (k2 args))
	;; +
	(lambda-proc (args env2 handler k2) (k2 (apply + args)))
	;; - 
	(lambda-proc (args env2 handler k2) (k2 (apply - args)))
	;; *
	(lambda-proc (args env2 handler k2) (k2 (apply * args)))
	;; /
	(lambda-proc (args env2 handler k2)
          (cond
            ((= (length args) 1)
             (if (= (car args) 0)
                 (handler "division by zero")
                 (k2 (apply / args))))
            ((>= (length args) 2)
             (if (= (cadr args) 0)
                 (handler "division by zero")
                 (k2 (apply / args))))
            (else (handler "not enough args to /"))))
	;; <
	(lambda-proc (args env2 handler k2) (k2 (apply < args)))
	;; >
	(lambda-proc (args env2 handler k2) (k2 (apply > args)))
	;; =
	(lambda-proc (args env2 handler k2) (k2 (apply = args)))
	;; equal?
	(lambda-proc (args env2 handler k2) (k2 (apply equal? args)))
	;; eq?
	(lambda-proc (args env2 handler k2) (k2 (apply eq? args)))
	;; memq
	(lambda-proc (args env2 handler k2) (k2 (apply memq args)))
	;; range
	(lambda-proc (args env2 handler k2) (k2 (apply range args)))
	;; set-car!
	(lambda-proc (args env2 handler k2) (k2 (apply set-car! args)))
	;; set-cdr
	(lambda-proc (args env2 handler k2) (k2 (apply set-cdr! args)))
	;; import
	(lambda-proc (args env2 handler k2) (import-primitive args env2 handler k2))
	;; get
	(lambda-proc (args env2 handler k2) (get-primitive args env2 handler k2))
	;; call/cc
	(lambda-proc (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	;; call/cc
	(lambda-proc (args env2 handler k2) (call/cc-primitive (car args) env2 handler k2))
	;; reverse
	(lambda-proc (args env2 handler k2) (k2 (apply reverse args)))
	;; append
	(lambda-proc (args env2 handler k2) (k2 (apply append args)))
	;; list->vector
	(lambda-proc (args env2 handler k2) (k2 (apply make-vector args)))
	;; dir
	(lambda-proc (args env2 handler k2) (k2 (dir args env2)))
	;; current-time
	(lambda-proc (args env2 handler k2) (k2 (get-current-time)))
	;; map
	(lambda-proc (args env2 handler k2)
	  (map-prim (car args) (cdr args) env2 handler k2))
	;; for-each
	(lambda-proc (args env2 handler k2)
	  (for-each-prim (car args) (cdr args) env2 handler k2))
	;; env
	(lambda-proc (args env2 handler k2) (k2 env2))
	;; using (not defined in scheme-scheme)
	(lambda-proc (args env2 handler k2) (k2 (using-prim args env2)))
	;; not
	(lambda-proc (args env2 handler k2) (k2 (not (car args))))
	;; printf
	(lambda-proc (args env2 handler k2) (apply printf-prim args) (k2 '<void>))
        ;; vector
	(lambda-proc (args env2 handler k2) (k2 (make-vector args)))
        ;; vector-set!
	(lambda-proc (args env2 handler k2) (k2 (vector-set! (car args) (cadr args) (caddr args))))
        ;; vector-ref
	(lambda-proc (args env2 handler k2) (k2 (apply vector-ref args)))
        ;; make-vector
	(lambda-proc (args env2 handler k2) (k2 (make-vector-size (car args))))
	
        ;; help
	(lambda-proc (args env2 handler k2) (help-prim (car args) 
						       env2 handler k2))
	)))))

;; supports procedures of any number of arguments
(define* map-prim
  (lambda (proc args env handler k)
    (let ((len (length args))
	  (list-args (listify args)))
      (cond
       ((= len 1) (map1 proc (car list-args) env handler k))
       ((= len 2) (map2 proc (car list-args) (cadr list-args) env handler k))
       (else (mapN proc list-args env handler k))))))

(define listify
  (lambda (arg-list)
    (cond
     ((null? arg-list) '())
     ((list? (car arg-list)) (cons (car arg-list)
				   (listify (cdr arg-list))))
     ((vector? (car arg-list)) (cons (my-vector->list (car arg-list))
				     (listify (cdr arg-list))))
     ((string? (car arg-list)) (cons (string->list (car arg-list))
				     (listify (cdr arg-list))))
     (else (error 'map "cannot use object type '~a' in map" 
		  (get_type (car arg-list))))))) ;; get_type is defined in C#

;; for improved efficiency
(define* map1
  (lambda (proc list1 env handler k)
    (if (null? list1)
      (k '())
      (if (dlr-exp? proc)
	  (map1 proc (cdr list1) env handler
		(lambda-cont (v2)
			     (k (cons (dlr-apply proc (list (car list1)))
				      v2))))
	  (proc (list (car list1)) env handler
		(lambda-cont (v1)
			     (map1 proc (cdr list1) env handler
				   (lambda-cont (v2)
						(k (cons v1 v2))))))))))

;; for improved efficiency
(define* map2
  (lambda (proc list1 list2 env handler k)
    (if (null? list1)
      (k '())
      (if (dlr-exp? proc)
	  (map2 proc (cdr list1) (cdr list2) env handler
		(lambda-cont (v2)
			     (k (cons (dlr-apply proc (list (car list1) (car list2)))
				      v2))))
	  (proc (list (car list1) (car list2)) env handler
		(lambda-cont (v1)
			     (map2 proc (cdr list1) (cdr list2) env handler
				   (lambda-cont (v2)
						(k (cons v1 v2))))))))))

(define* mapN
  (lambda (proc lists env handler k)
    (if (null? (car lists))
      (k '())
      (if (dlr-exp? proc)
	  (mapN proc (map cdr lists) env handler
		(lambda-cont (v2)
		    (k (cons (dlr-apply proc (map car lists)) v2))))
	  (proc (map car lists) env handler
		(lambda-cont (v1)
			     (mapN proc (map cdr lists) env handler
				   (lambda-cont (v2)
						(k (cons v1 v2))))))))))

(define* for-each-prim
  (lambda (proc lists env handler k)
    (let ((arg-list (listify lists)))
      (if (null? (car arg-list))
	  (k '<void>)
	  (if (dlr-exp? proc) 
	      (begin 
		(dlr-apply proc (map car arg-list))
		(for-each-prim proc (map cdr arg-list) env handler k))
	      (proc (map car arg-list) env handler
		    (lambda-cont (v1)
				 (for-each-prim proc (map cdr arg-list) env handler k))))))))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now)
		     1000000000))))))

(define* get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value sym env handler
	(lambda-cont (v)
	  (cond
	    ((null? (cdr args)) (k v))
	    ((not (environment? v)) (handler (format "~a is not a module" sym)))
	    (else (get-primitive (cdr args) v handler k))))))))

;; bug fix needed:
;; (import "my-fact.ss" 'm)
;; (m.m.m.m.m.fib 10) =>  89

(define* import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file filename env handler k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler
	      (lambda-cont (binding)
		(let ((module (extend env '() '())))
		  (set-binding-value! binding module)
		  (load-file filename module handler k)))))))))

(define* call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (lambda-proc (args env2 handler k2) (k (car args)))))
      (if (dlr-exp? proc)
	  (k (dlr-apply proc (list fake-k)))
	  (proc (list fake-k) env handler k)))))

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

(define dir
  (lambda (args env)
    (sort symbol<? (if (null? args)
		       (flatten 
			(append
			 (map get-variables-from-frame (frames macro-env))
			 (map get-variables-from-frame (frames env))))
			(get-variables-from-frame (car (frames (car args))))))))

(define get-variables-from-frame
  (lambda (frame) 
    (map binding-variable frame)))

(define symbol<?
  (lambda (a b)
    (let ((a_string (symbol->string a))
	  (b_string (symbol->string b)))
      (string<? a_string b_string))))

(define load-stack '())

(define* load-file
  (lambda (filename env handler k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k '<void>))
      ((not (string? filename))
       (handler (format "filename is not a string: ~a" filename)))
      ((not (file-exists? filename))
       (handler (format "file does not exist: ~a" filename)))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) handler
	 (lambda-cont (tokens)
	   (load-loop tokens env handler
	     (lambda-cont (v)
	       (set! load-stack (cdr load-stack))
	       (k v)))))))))

(define* load-loop
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
      (k '<void>)
      (read-sexp tokens handler
	(lambda-cont2 (datum tokens-left)
	  (parse datum handler
	    (lambda-cont (exp)
	      (m exp env handler
		(lambda-cont (v)
		  (load-loop tokens-left env handler k))))))))))

(define* load-files
  (lambda (filenames env handler k)
    (if (null? filenames)
      (k 'ok)
      (load-file (car filenames) env handler
	(lambda-cont (v)
	  (load-files (cdr filenames) env handler k))))))

(define* help-prim
  (lambda (var env handler k)
    (lookup-binding var env handler
	 (lambda-cont (binding)
	     (k (binding-docstring binding))))))

(define range
  (lambda args
    (letrec
	((range
	  (lambda (n end step acc)
	    (if (>= n end)
	      (reverse acc)
	      (range (+ n step) end step (cons n acc))))))
      (cond
	((null? (cdr args)) (range 0 (car args) 1 '()))
	((null? (cddr args)) (range (car args) (cadr args) 1 '()))
	(else (range (car args) (cadr args) (caddr args) '()))))))
	
(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 handler k2)
      (k2 (apply* external-function-object args)))))

(define make-vector list->vector) ;; ignored in C#

(define toplevel-env (make-toplevel-env))
(define macro-env (make-macro-env))

;; For C# only
(define Main 
  (lambda (args)
    (printf "Pyjama Scheme (0.1)\n")
    (printf "(c) 2009, IPRE\n")
    ;; in the register machine, this call just sets up the registers
    (load-files (list args) toplevel-env REP-handler REP-k)
    ;; need this to start the computation after registers are set up
    (trampoline)))

(define execute
  (lambda (input-string)
      (read-datum input-string init-handler
	(lambda-cont2 (datum tokens-left)
	  (parse datum init-handler
	    (lambda-cont (exp)
	      (m exp toplevel-env init-handler init-cont)))))
      (trampoline)))
(load "transformer-macros.ss")

;; Unification pattern-matcher

(define pattern?
  (lambda (x)
    (or (null? x)
	(number? x)
	(boolean? x)
	(symbol? x)
	(and (pair? x)
	     (pattern? (car x))
	     (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
	 (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x))
	 (not (pair? x)))))

(define* occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (k #f))
      ((pattern-variable? pattern) (k (equal? var pattern)))
      (else (occurs? var (car pattern)
	      (lambda-cont (bool)
		(if bool
		  (k #t)
		  (occurs? var (cdr pattern) k))))))))

(define* unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (make-sub 'unit p1 p2))
	 (occurs? p1 p2
	   (lambda-cont (bool)
	     (if bool
	       (k #f)
	       (k (make-sub 'unit p1 p2)))))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (k #f)))))

(define* unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns (car pair1) (car pair2)
      (lambda-cont (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate (cdr pair1) s-car
	    (lambda-cont (new-cdr1)
	      (instantiate (cdr pair2) s-car
		(lambda-cont (new-cdr2)
		  (unify-patterns new-cdr1 new-cdr2
		    (lambda-cont (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (make-sub 'composite s-car s-cdr))))))))))))))

(define* instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate (car pattern) s
	 (lambda-cont (a)
	   (instantiate (cdr pattern) s
	     (lambda-cont (b)
	       (k (cons a b)))))))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

;;(define extend-sub
;;  (lambda (old-s new-var new-pattern)
;;    (list 'extended new-var new-pattern old-s)))

(define* apply-sub
  (lambda (s var k)
    (record-case (cdr s)
      (empty () (k var))
;;      (extended (new-var new-pattern old-s)
;;	(if (equal? var new-var)
;;	  (k new-pattern)
;;	  (apply-sub old-s var k)))
      (unit (new-var new-pattern)
	(if (equal? var new-var)
	  (k new-pattern)
	  (k var)))
      (composite (s1 s2)
	(apply-sub s1 var
	  (lambda-cont (pattern)
	    (instantiate pattern s2 k))))
      (else (error 'apply-sub "bad substitution: ~a" s)))))
