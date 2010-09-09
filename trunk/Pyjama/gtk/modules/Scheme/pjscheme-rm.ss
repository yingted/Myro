(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; EOPL support

;; expression datatype
(define lit-exp
  (lambda args (return* (cons 'lit-exp args))))
(define var-exp
  (lambda args (return* (cons 'var-exp args))))
(define if-exp (lambda args (return* (cons 'if-exp args))))
(define assign-exp
  (lambda args (return* (cons 'assign-exp args))))
(define define-exp
  (lambda args (return* (cons 'define-exp args))))
(define define-syntax-exp
  (lambda args (return* (cons 'define-syntax-exp args))))
(define begin-exp
  (lambda args (return* (cons 'begin-exp args))))
(define lambda-exp
  (lambda args (return* (cons 'lambda-exp args))))
(define mu-lambda-exp
  (lambda args (return* (cons 'mu-lambda-exp args))))
(define app-exp
  (lambda args (return* (cons 'app-exp args))))
(define try-catch-exp
  (lambda args (return* (cons 'try-catch-exp args))))
(define try-finally-exp
  (lambda args (return* (cons 'try-finally-exp args))))
(define try-catch-finally-exp
  (lambda args (return* (cons 'try-catch-finally-exp args))))
(define raise-exp
  (lambda args (return* (cons 'raise-exp args))))
(define dict-exp
  (lambda args (return* (cons 'dict-exp args))))

;;----------------------------------------------------------------------

;; global registers
(define pc 'undefined)
(define action_reg 'undefined)
(define args_reg 'undefined)
(define bindings_reg 'undefined)
(define bodies_reg 'undefined)
(define buffer_reg 'undefined)
(define chars_reg 'undefined)
(define clauses_reg 'undefined)
(define components_reg 'undefined)
(define datum-list_reg 'undefined)
(define datum_reg 'undefined)
(define env2_reg 'undefined)
(define env_reg 'undefined)
(define exception_reg 'undefined)
(define exp_reg 'undefined)
(define expected-terminator_reg 'undefined)
(define exps_reg 'undefined)
(define filename_reg 'undefined)
(define filenames_reg 'undefined)
(define final_reg 'undefined)
(define handler_reg 'undefined)
(define input_reg 'undefined)
(define k2_reg 'undefined)
(define k_reg 'undefined)
(define keyword_reg 'undefined)
(define list1_reg 'undefined)
(define list2_reg 'undefined)
(define lists_reg 'undefined)
(define macro_reg 'undefined)
(define p1_reg 'undefined)
(define p2_reg 'undefined)
(define pair1_reg 'undefined)
(define pair2_reg 'undefined)
(define pairs_reg 'undefined)
(define path_reg 'undefined)
(define pattern_reg 'undefined)
(define proc_reg 'undefined)
(define procs_reg 'undefined)
(define s_reg 'undefined)
(define sexp_reg 'undefined)
(define token-type_reg 'undefined)
(define tokens_reg 'undefined)
(define value1_reg 'undefined)
(define value2_reg 'undefined)
(define value_reg 'undefined)
(define var_reg 'undefined)
(define variable_reg 'undefined)
(define vars_reg 'undefined)

;; temporary registers
(define temp_2 'undefined)
(define temp_1 'undefined)

(define make-cont
  (lambda args (return* (cons 'continuation args))))

(define*
  apply-cont
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr k_reg))
      (if (eq? (car temp_1) '<cont-1>)
          (let ((token 'undefined) (k 'undefined))
            (set! k (list-ref temp_1 2))
            (set! token (list-ref temp_1 1))
            (set! value_reg (cons token value_reg))
            (set! k_reg k)
            (set! pc apply-cont))
          (if (eq? (car temp_1) '<cont-2>)
              (begin (set! final_reg value_reg) (set! pc #f))
              (if (eq? (car temp_1) '<cont-3>)
                  (let ((chars 'undefined) (k 'undefined))
                    (set! k (list-ref temp_1 2))
                    (set! chars (list-ref temp_1 1))
                    (set! value2_reg chars)
                    (set! value1_reg
                      (append value_reg (list read-line-count read-char-count)))
                    (set! k_reg k)
                    (set! pc apply-cont2))
                  (if (eq? (car temp_1) '<cont-4>)
                      (let ((handler 'undefined) (k 'undefined))
                        (set! k (list-ref temp_1 2))
                        (set! handler (list-ref temp_1 1))
                        (set! k_reg (make-cont2 '<cont2-3> handler k))
                        (set! handler_reg handler)
                        (set! tokens_reg value_reg)
                        (set! pc read-sexp))
                      (if (eq? (car temp_1) '<cont-5>)
                          (begin
                            (set! k_reg init-cont)
                            (set! handler_reg init-handler)
                            (set! tokens_reg value_reg)
                            (set! pc print-unparsed-sexps))
                          (if (eq? (car temp_1) '<cont-6>)
                              (let ((k 'undefined))
                                (set! k (list-ref temp_1 1))
                                (set! value_reg (binding-value value_reg))
                                (set! k_reg k)
                                (set! pc apply-cont))
                              (if (eq? (car temp_1) '<cont-7>)
                                  (let ((variable 'undefined)
                                        (env 'undefined)
                                        (handler 'undefined)
                                        (k 'undefined))
                                    (set! k (list-ref temp_1 4))
                                    (set! handler (list-ref temp_1 3))
                                    (set! env (list-ref temp_1 2))
                                    (set! variable (list-ref temp_1 1))
                                    (if value_reg
                                        (begin
                                          (set! k_reg k)
                                          (set! handler_reg handler)
                                          (set! env_reg env)
                                          (set! path_reg "")
                                          (set! components_reg value_reg)
                                          (set! pc lookup-variable-components))
                                        (if (dlr-env-contains variable)
                                            (begin
                                              (set! value_reg (dlr-env-lookup variable))
                                              (set! k_reg k)
                                              (set! pc apply-cont))
                                            (begin
                                              (set! exception_reg (format "unbound variable ~a" variable))
                                              (set! handler_reg handler)
                                              (set! pc apply-handler)))))
                                  (if (eq? (car temp_1) '<cont-8>)
                                      (let ((components 'undefined)
                                            (path 'undefined)
                                            (var 'undefined)
                                            (handler 'undefined)
                                            (k 'undefined))
                                        (set! k (list-ref temp_1 5))
                                        (set! handler (list-ref temp_1 4))
                                        (set! var (list-ref temp_1 3))
                                        (set! path (list-ref temp_1 2))
                                        (set! components (list-ref temp_1 1))
                                        (if (null? (cdr components))
                                            (begin (set! k_reg k) (set! pc apply-cont))
                                            (let ((result 'undefined) (new-path 'undefined))
                                              (set! new-path
                                                (if (string=? path "")
                                                    (format "~a" var)
                                                    (format "~a.~a" path var)))
                                              (set! result (binding-value value_reg))
                                              (if (not (environment? result))
                                                  (begin
                                                    (set! exception_reg (format "~a is not a module" new-path))
                                                    (set! handler_reg handler)
                                                    (set! pc apply-handler))
                                                  (begin
                                                    (set! k_reg k)
                                                    (set! handler_reg handler)
                                                    (set! env_reg result)
                                                    (set! path_reg new-path)
                                                    (set! components_reg (cdr components))
                                                    (set! pc lookup-variable-components))))))
                                      (if (eq? (car temp_1) '<cont-9>)
                                          (let ((datum 'undefined)
                                                (handler 'undefined)
                                                (k 'undefined))
                                            (set! k (list-ref temp_1 3))
                                            (set! handler (list-ref temp_1 2))
                                            (set! datum (list-ref temp_1 1))
                                            (if (pattern-macro? value_reg)
                                                (begin
                                                  (set! k_reg k)
                                                  (set! handler_reg handler)
                                                  (set! datum_reg datum)
                                                  (set! clauses_reg (macro-clauses value_reg))
                                                  (set! pc process-macro-clauses))
                                                (begin
                                                  (set! k_reg k)
                                                  (set! datum_reg datum)
                                                  (set! macro_reg value_reg)
                                                  (set! pc apply-macro))))
                                          (if (eq? (car temp_1) '<cont-10>)
                                              (let ((clauses 'undefined)
                                                    (datum 'undefined)
                                                    (right-pattern 'undefined)
                                                    (handler 'undefined)
                                                    (k 'undefined))
                                                (set! k (list-ref temp_1 5))
                                                (set! handler (list-ref temp_1 4))
                                                (set! right-pattern (list-ref temp_1 3))
                                                (set! datum (list-ref temp_1 2))
                                                (set! clauses (list-ref temp_1 1))
                                                (if value_reg
                                                    (begin
                                                      (set! k_reg k)
                                                      (set! s_reg value_reg)
                                                      (set! pattern_reg right-pattern)
                                                      (set! pc instantiate))
                                                    (begin
                                                      (set! k_reg k)
                                                      (set! handler_reg handler)
                                                      (set! datum_reg datum)
                                                      (set! clauses_reg (cdr clauses))
                                                      (set! pc process-macro-clauses))))
                                              (if (eq? (car temp_1) '<cont-11>)
                                                  (let ((bindings 'undefined) (k 'undefined))
                                                    (set! k (list-ref temp_1 2))
                                                    (set! bindings (list-ref temp_1 1))
                                                    (set! value_reg (list 'let (list (car bindings)) value_reg))
                                                    (set! k_reg k)
                                                    (set! pc apply-cont))
                                                  (if (eq? (car temp_1) '<cont-12>)
                                                      (let ((k 'undefined))
                                                        (set! k (list-ref temp_1 1))
                                                        (set! value_reg (cons 'cond value_reg))
                                                        (set! k_reg k)
                                                        (set! pc apply-cont))
                                                      (if (eq? (car temp_1) '<cont-13>)
                                                          (let ((clauses 'undefined) (var 'undefined) (k 'undefined))
                                                            (set! k (list-ref temp_1 3))
                                                            (set! var (list-ref temp_1 2))
                                                            (set! clauses (list-ref temp_1 1))
                                                            (let ((clause 'undefined))
                                                              (set! clause (car clauses))
                                                              (if (eq? (car clause) 'else)
                                                                  (begin
                                                                    (set! value_reg (cons clause value_reg))
                                                                    (set! k_reg k)
                                                                    (set! pc apply-cont))
                                                                  (if (symbol? (car clause))
                                                                      (begin
                                                                        (set! value_reg
                                                                          (cons
                                                                            (cons
                                                                              (list 'eq? var (list 'quote (car clause)))
                                                                              (cdr clause))
                                                                            value_reg))
                                                                        (set! k_reg k)
                                                                        (set! pc apply-cont))
                                                                      (begin
                                                                        (set! value_reg
                                                                          (cons
                                                                            (cons
                                                                              (list 'memq var (list 'quote (car clause)))
                                                                              (cdr clause))
                                                                            value_reg))
                                                                        (set! k_reg k)
                                                                        (set! pc apply-cont))))))
                                                          (if (eq? (car temp_1) '<cont-14>)
                                                              (let ((v1 'undefined) (k 'undefined))
                                                                (set! k (list-ref temp_1 2))
                                                                (set! v1 (list-ref temp_1 1))
                                                                (set! value_reg (app-exp v1 value_reg))
                                                                (set! k_reg k)
                                                                (set! pc apply-cont))
                                                              (if (eq? (car temp_1) '<cont-15>)
                                                                  (let ((datum 'undefined)
                                                                        (handler 'undefined)
                                                                        (k 'undefined))
                                                                    (set! k (list-ref temp_1 3))
                                                                    (set! handler (list-ref temp_1 2))
                                                                    (set! datum (list-ref temp_1 1))
                                                                    (set! k_reg (make-cont '<cont-14> value_reg k))
                                                                    (set! handler_reg handler)
                                                                    (set! datum-list_reg (cdr datum))
                                                                    (set! pc parse-all))
                                                                  (if (eq? (car temp_1) '<cont-16>)
                                                                      (let ((k 'undefined))
                                                                        (set! k (list-ref temp_1 1))
                                                                        (set! value_reg (dict-exp value_reg))
                                                                        (set! k_reg k)
                                                                        (set! pc apply-cont))
                                                                      (if (eq? (car temp_1) '<cont-17>)
                                                                          (let ((k 'undefined))
                                                                            (set! k (list-ref temp_1 1))
                                                                            (set! value_reg (raise-exp value_reg))
                                                                            (set! k_reg k)
                                                                            (set! pc apply-cont))
                                                                          (if (eq? (car temp_1) '<cont-18>)
                                                                              (let ((cexps 'undefined)
                                                                                    (datum 'undefined)
                                                                                    (body 'undefined)
                                                                                    (k 'undefined))
                                                                                (set! k (list-ref temp_1 4))
                                                                                (set! body (list-ref temp_1 3))
                                                                                (set! datum (list-ref temp_1 2))
                                                                                (set! cexps (list-ref temp_1 1))
                                                                                (let ((cvar 'undefined))
                                                                                  (set! cvar (catch-var (caddr datum)))
                                                                                  (set! value_reg
                                                                                    (try-catch-finally-exp body cvar cexps value_reg))
                                                                                  (set! k_reg k)
                                                                                  (set! pc apply-cont)))
                                                                              (if (eq? (car temp_1) '<cont-19>)
                                                                                  (let ((datum 'undefined)
                                                                                        (body 'undefined)
                                                                                        (handler 'undefined)
                                                                                        (k 'undefined))
                                                                                    (set! k (list-ref temp_1 4))
                                                                                    (set! handler (list-ref temp_1 3))
                                                                                    (set! body (list-ref temp_1 2))
                                                                                    (set! datum (list-ref temp_1 1))
                                                                                    (set! k_reg (make-cont '<cont-18> value_reg datum body k))
                                                                                    (set! handler_reg handler)
                                                                                    (set! datum-list_reg (finally-exps (cadddr datum)))
                                                                                    (set! pc parse-all))
                                                                                  (if (eq? (car temp_1) '<cont-20>)
                                                                                      (let ((datum 'undefined)
                                                                                            (handler 'undefined)
                                                                                            (k 'undefined))
                                                                                        (set! k (list-ref temp_1 3))
                                                                                        (set! handler (list-ref temp_1 2))
                                                                                        (set! datum (list-ref temp_1 1))
                                                                                        (set! k_reg
                                                                                          (make-cont '<cont-19> datum value_reg handler k))
                                                                                        (set! handler_reg handler)
                                                                                        (set! datum-list_reg (catch-exps (caddr datum)))
                                                                                        (set! pc parse-all))
                                                                                      (if (eq? (car temp_1) '<cont-21>)
                                                                                          (let ((body 'undefined) (k 'undefined))
                                                                                            (set! k (list-ref temp_1 2))
                                                                                            (set! body (list-ref temp_1 1))
                                                                                            (set! value_reg (try-finally-exp body value_reg))
                                                                                            (set! k_reg k)
                                                                                            (set! pc apply-cont))
                                                                                          (if (eq? (car temp_1) '<cont-22>)
                                                                                              (let ((datum 'undefined)
                                                                                                    (handler 'undefined)
                                                                                                    (k 'undefined))
                                                                                                (set! k (list-ref temp_1 3))
                                                                                                (set! handler (list-ref temp_1 2))
                                                                                                (set! datum (list-ref temp_1 1))
                                                                                                (set! k_reg (make-cont '<cont-21> value_reg k))
                                                                                                (set! handler_reg handler)
                                                                                                (set! datum-list_reg (finally-exps (caddr datum)))
                                                                                                (set! pc parse-all))
                                                                                              (if (eq? (car temp_1) '<cont-23>)
                                                                                                  (let ((datum 'undefined) (body 'undefined) (k 'undefined))
                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                    (set! body (list-ref temp_1 2))
                                                                                                    (set! datum (list-ref temp_1 1))
                                                                                                    (let ((cvar 'undefined))
                                                                                                      (set! cvar (catch-var (caddr datum)))
                                                                                                      (set! value_reg (try-catch-exp body cvar value_reg))
                                                                                                      (set! k_reg k)
                                                                                                      (set! pc apply-cont)))
                                                                                                  (if (eq? (car temp_1) '<cont-24>)
                                                                                                      (let ((datum 'undefined)
                                                                                                            (handler 'undefined)
                                                                                                            (k 'undefined))
                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                        (set! handler (list-ref temp_1 2))
                                                                                                        (set! datum (list-ref temp_1 1))
                                                                                                        (set! k_reg (make-cont '<cont-23> datum value_reg k))
                                                                                                        (set! handler_reg handler)
                                                                                                        (set! datum-list_reg (catch-exps (caddr datum)))
                                                                                                        (set! pc parse-all))
                                                                                                      (if (eq? (car temp_1) '<cont-25>)
                                                                                                          (let ((datum 'undefined) (k 'undefined))
                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                            (set! datum (list-ref temp_1 1))
                                                                                                            (if (list? (cadr datum))
                                                                                                                (begin
                                                                                                                  (set! value_reg (lambda-exp (cadr datum) value_reg))
                                                                                                                  (set! k_reg k)
                                                                                                                  (set! pc apply-cont))
                                                                                                                (begin
                                                                                                                  (set! value_reg
                                                                                                                    (mu-lambda-exp
                                                                                                                      (head (cadr datum))
                                                                                                                      (last (cadr datum))
                                                                                                                      value_reg))
                                                                                                                  (set! k_reg k)
                                                                                                                  (set! pc apply-cont))))
                                                                                                          (if (eq? (car temp_1) '<cont-26>)
                                                                                                              (let ((datum 'undefined)
                                                                                                                    (handler 'undefined)
                                                                                                                    (k 'undefined))
                                                                                                                (set! k (list-ref temp_1 3))
                                                                                                                (set! handler (list-ref temp_1 2))
                                                                                                                (set! datum (list-ref temp_1 1))
                                                                                                                (if (null? value_reg)
                                                                                                                    (begin
                                                                                                                      (set! exception_reg
                                                                                                                        (format "bad concrete syntax: ~a" datum))
                                                                                                                      (set! handler_reg handler)
                                                                                                                      (set! pc apply-handler))
                                                                                                                    (if (null? (cdr value_reg))
                                                                                                                        (begin
                                                                                                                          (set! value_reg (car value_reg))
                                                                                                                          (set! k_reg k)
                                                                                                                          (set! pc apply-cont))
                                                                                                                        (begin
                                                                                                                          (set! value_reg (begin-exp value_reg))
                                                                                                                          (set! k_reg k)
                                                                                                                          (set! pc apply-cont)))))
                                                                                                              (if (eq? (car temp_1) '<cont-27>)
                                                                                                                  (let ((datum 'undefined) (body 'undefined) (k 'undefined))
                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                    (set! body (list-ref temp_1 2))
                                                                                                                    (set! datum (list-ref temp_1 1))
                                                                                                                    (set! value_reg
                                                                                                                      (define-exp (cadr datum) (list value_reg body)))
                                                                                                                    (set! k_reg k)
                                                                                                                    (set! pc apply-cont))
                                                                                                                  (if (eq? (car temp_1) '<cont-28>)
                                                                                                                      (let ((datum 'undefined)
                                                                                                                            (handler 'undefined)
                                                                                                                            (k 'undefined))
                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                        (set! handler (list-ref temp_1 2))
                                                                                                                        (set! datum (list-ref temp_1 1))
                                                                                                                        (set! k_reg (make-cont '<cont-27> datum value_reg k))
                                                                                                                        (set! handler_reg handler)
                                                                                                                        (set! datum_reg (caddr datum))
                                                                                                                        (set! pc parse))
                                                                                                                      (if (eq? (car temp_1) '<cont-29>)
                                                                                                                          (let ((datum 'undefined) (k 'undefined))
                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                            (set! datum (list-ref temp_1 1))
                                                                                                                            (set! value_reg (define-exp (cadr datum) (list value_reg)))
                                                                                                                            (set! k_reg k)
                                                                                                                            (set! pc apply-cont))
                                                                                                                          (if (eq? (car temp_1) '<cont-30>)
                                                                                                                              (let ((handler 'undefined) (k 'undefined))
                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                (set! handler (list-ref temp_1 1))
                                                                                                                                (set! k_reg k)
                                                                                                                                (set! handler_reg handler)
                                                                                                                                (set! datum_reg value_reg)
                                                                                                                                (set! pc parse))
                                                                                                                              (if (eq? (car temp_1) '<cont-31>)
                                                                                                                                  (let ((datum 'undefined) (k 'undefined))
                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                    (set! datum (list-ref temp_1 1))
                                                                                                                                    (set! value_reg (assign-exp (cadr datum) value_reg))
                                                                                                                                    (set! k_reg k)
                                                                                                                                    (set! pc apply-cont))
                                                                                                                                  (if (eq? (car temp_1) '<cont-32>)
                                                                                                                                      (let ((v1 'undefined) (v2 'undefined) (k 'undefined))
                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                        (set! v2 (list-ref temp_1 2))
                                                                                                                                        (set! v1 (list-ref temp_1 1))
                                                                                                                                        (set! value_reg (if-exp v1 v2 value_reg))
                                                                                                                                        (set! k_reg k)
                                                                                                                                        (set! pc apply-cont))
                                                                                                                                      (if (eq? (car temp_1) '<cont-33>)
                                                                                                                                          (let ((datum 'undefined)
                                                                                                                                                (v1 'undefined)
                                                                                                                                                (handler 'undefined)
                                                                                                                                                (k 'undefined))
                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                            (set! handler (list-ref temp_1 3))
                                                                                                                                            (set! v1 (list-ref temp_1 2))
                                                                                                                                            (set! datum (list-ref temp_1 1))
                                                                                                                                            (set! k_reg (make-cont '<cont-32> v1 value_reg k))
                                                                                                                                            (set! handler_reg handler)
                                                                                                                                            (set! datum_reg (cadddr datum))
                                                                                                                                            (set! pc parse))
                                                                                                                                          (if (eq? (car temp_1) '<cont-34>)
                                                                                                                                              (let ((datum 'undefined)
                                                                                                                                                    (handler 'undefined)
                                                                                                                                                    (k 'undefined))
                                                                                                                                                (set! k (list-ref temp_1 3))
                                                                                                                                                (set! handler (list-ref temp_1 2))
                                                                                                                                                (set! datum (list-ref temp_1 1))
                                                                                                                                                (set! k_reg
                                                                                                                                                  (make-cont '<cont-33> datum value_reg handler k))
                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                (set! datum_reg (caddr datum))
                                                                                                                                                (set! pc parse))
                                                                                                                                              (if (eq? (car temp_1) '<cont-35>)
                                                                                                                                                  (let ((v1 'undefined) (k 'undefined))
                                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                                    (set! v1 (list-ref temp_1 1))
                                                                                                                                                    (set! value_reg (if-exp v1 value_reg (lit-exp #f)))
                                                                                                                                                    (set! k_reg k)
                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                  (if (eq? (car temp_1) '<cont-36>)
                                                                                                                                                      (let ((datum 'undefined)
                                                                                                                                                            (handler 'undefined)
                                                                                                                                                            (k 'undefined))
                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                        (set! handler (list-ref temp_1 2))
                                                                                                                                                        (set! datum (list-ref temp_1 1))
                                                                                                                                                        (set! k_reg (make-cont '<cont-35> value_reg k))
                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                        (set! datum_reg (caddr datum))
                                                                                                                                                        (set! pc parse))
                                                                                                                                                      (if (eq? (car temp_1) '<cont-37>)
                                                                                                                                                          (let ((a 'undefined) (b 'undefined) (k 'undefined))
                                                                                                                                                            (set! k (list-ref temp_1 3))
                                                                                                                                                            (set! b (list-ref temp_1 2))
                                                                                                                                                            (set! a (list-ref temp_1 1))
                                                                                                                                                            (set! value_reg (cons (list a b) value_reg))
                                                                                                                                                            (set! k_reg k)
                                                                                                                                                            (set! pc apply-cont))
                                                                                                                                                          (if (eq? (car temp_1) '<cont-38>)
                                                                                                                                                              (let ((a 'undefined)
                                                                                                                                                                    (pairs 'undefined)
                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                (set! pairs (list-ref temp_1 2))
                                                                                                                                                                (set! a (list-ref temp_1 1))
                                                                                                                                                                (set! k_reg (make-cont '<cont-37> a value_reg k))
                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                (set! pairs_reg (cdr pairs))
                                                                                                                                                                (set! pc parse-pairs))
                                                                                                                                                              (if (eq? (car temp_1) '<cont-39>)
                                                                                                                                                                  (let ((pairs 'undefined)
                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                    (set! pairs (list-ref temp_1 1))
                                                                                                                                                                    (set! k_reg
                                                                                                                                                                      (make-cont '<cont-38> value_reg pairs handler k))
                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                    (set! datum_reg (cadar pairs))
                                                                                                                                                                    (set! pc parse))
                                                                                                                                                                  (if (eq? (car temp_1) '<cont-40>)
                                                                                                                                                                      (let ((a 'undefined) (k 'undefined))
                                                                                                                                                                        (set! k (list-ref temp_1 2))
                                                                                                                                                                        (set! a (list-ref temp_1 1))
                                                                                                                                                                        (set! value_reg (cons a value_reg))
                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                      (if (eq? (car temp_1) '<cont-41>)
                                                                                                                                                                          (let ((datum-list 'undefined)
                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                            (set! k (list-ref temp_1 3))
                                                                                                                                                                            (set! handler (list-ref temp_1 2))
                                                                                                                                                                            (set! datum-list (list-ref temp_1 1))
                                                                                                                                                                            (set! k_reg (make-cont '<cont-40> value_reg k))
                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                            (set! datum-list_reg (cdr datum-list))
                                                                                                                                                                            (set! pc parse-all))
                                                                                                                                                                          (if (eq? (car temp_1) '<cont-42>)
                                                                                                                                                                              (let ((v1 'undefined) (k 'undefined))
                                                                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                                                                (set! v1 (list-ref temp_1 1))
                                                                                                                                                                                (set! value_reg (list 'cons v1 value_reg))
                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                              (if (eq? (car temp_1) '<cont-43>)
                                                                                                                                                                                  (let ((datum 'undefined)
                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                                    (set! datum (list-ref temp_1 1))
                                                                                                                                                                                    (set! k_reg (make-cont '<cont-42> value_reg k))
                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                    (set! datum_reg (cdr datum))
                                                                                                                                                                                    (set! pc expand-quasiquote))
                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-44>)
                                                                                                                                                                                      (let ((k 'undefined))
                                                                                                                                                                                        (set! k (list-ref temp_1 1))
                                                                                                                                                                                        (set! value_reg (cons 'list value_reg))
                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-45>)
                                                                                                                                                                                          (let ((datum 'undefined) (k 'undefined))
                                                                                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                                                                                            (set! datum (list-ref temp_1 1))
                                                                                                                                                                                            (set! value_reg (list 'append (cadr (car datum)) value_reg))
                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                            (set! pc apply-cont))
                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-46>)
                                                                                                                                                                                              (let ((k 'undefined))
                                                                                                                                                                                                (set! k (list-ref temp_1 1))
                                                                                                                                                                                                (set! value_reg (list 'list->vector value_reg))
                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-47>)
                                                                                                                                                                                                  (let ((v1 'undefined) (k 'undefined))
                                                                                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                                                                                    (set! v1 (list-ref temp_1 1))
                                                                                                                                                                                                    (set! value_reg (cons v1 value_reg))
                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-48>)
                                                                                                                                                                                                      (let ((datum 'undefined)
                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                                                                        (set! handler (list-ref temp_1 2))
                                                                                                                                                                                                        (set! datum (list-ref temp_1 1))
                                                                                                                                                                                                        (set! k_reg (make-cont '<cont-47> value_reg k))
                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                        (set! datum_reg (cdr datum))
                                                                                                                                                                                                        (set! pc expand-quasiquote-list))
                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-49>)
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! k_reg init-cont)
                                                                                                                                                                                                            (set! handler_reg init-handler)
                                                                                                                                                                                                            (set! tokens_reg value_reg)
                                                                                                                                                                                                            (set! pc parse-sexps))
                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-50>)
                                                                                                                                                                                                              (let ((exp 'undefined) (k 'undefined))
                                                                                                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                (set! exp (list-ref temp_1 1))
                                                                                                                                                                                                                (set! value_reg (cons exp value_reg))
                                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-51>)
                                                                                                                                                                                                                  (let ((tokens-left 'undefined)
                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                                                                    (set! tokens-left (list-ref temp_1 1))
                                                                                                                                                                                                                    (set! k_reg (make-cont '<cont-50> value_reg k))
                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                    (set! tokens_reg tokens-left)
                                                                                                                                                                                                                    (set! pc parse-sexps))
                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-52>)
                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                        (set! k_reg REP-k)
                                                                                                                                                                                                                        (set! handler_reg REP-handler)
                                                                                                                                                                                                                        (set! env_reg toplevel-env)
                                                                                                                                                                                                                        (set! exp_reg value_reg)
                                                                                                                                                                                                                        (set! pc m))
                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-53>)
                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                            (set! k_reg (make-cont2 '<cont2-20>))
                                                                                                                                                                                                                            (set! handler_reg REP-handler)
                                                                                                                                                                                                                            (set! input_reg "(exit)")
                                                                                                                                                                                                                            (set! pc read-datum))
                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-54>)
                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                (set! k_reg (make-cont '<cont-53>))
                                                                                                                                                                                                                                (set! handler_reg REP-handler)
                                                                                                                                                                                                                                (set! env_reg toplevel-env)
                                                                                                                                                                                                                                (set! exp_reg value_reg)
                                                                                                                                                                                                                                (set! pc m))
                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-55>)
                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                    (set! k_reg (make-cont2 '<cont2-21>))
                                                                                                                                                                                                                                    (set! handler_reg REP-handler)
                                                                                                                                                                                                                                    (set! input_reg "(test-all)")
                                                                                                                                                                                                                                    (set! pc read-datum))
                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-56>)
                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                        (set! k_reg (make-cont '<cont-55>))
                                                                                                                                                                                                                                        (set! handler_reg REP-handler)
                                                                                                                                                                                                                                        (set! env_reg toplevel-env)
                                                                                                                                                                                                                                        (set! exp_reg value_reg)
                                                                                                                                                                                                                                        (set! pc m))
                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-57>)
                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                            (if (not (eq? value_reg '<void>))
                                                                                                                                                                                                                                                (pretty-print-prim value_reg))
                                                                                                                                                                                                                                            (if *need-newline* (newline))
                                                                                                                                                                                                                                            (set! pc read-eval-print))
                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-58>)
                                                                                                                                                                                                                                              (let ((args 'undefined)
                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                (set! args (list-ref temp_1 1))
                                                                                                                                                                                                                                                (if (dlr-exp? value_reg)
                                                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                                                      (set! value_reg (dlr-apply value_reg args))
                                                                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                                                                      (set! pc apply-cont))
                                                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                                                      (set! k2_reg k)
                                                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                                                      (set! env2_reg env)
                                                                                                                                                                                                                                                      (set! args_reg args)
                                                                                                                                                                                                                                                      (set! proc_reg value_reg)
                                                                                                                                                                                                                                                      (set! pc apply-proc))))
                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-59>)
                                                                                                                                                                                                                                                  (let ((operator 'undefined)
                                                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                    (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                    (set! operator (list-ref temp_1 1))
                                                                                                                                                                                                                                                    (set! k_reg (make-cont '<cont-58> value_reg env handler k))
                                                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                                                    (set! env_reg env)
                                                                                                                                                                                                                                                    (set! exp_reg operator)
                                                                                                                                                                                                                                                    (set! pc m))
                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-60>)
                                                                                                                                                                                                                                                      (let ((handler 'undefined))
                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 1))
                                                                                                                                                                                                                                                        (set! exception_reg value_reg)
                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                        (set! pc apply-handler))
                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-61>)
                                                                                                                                                                                                                                                          (let ((v 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                                                            (set! v (list-ref temp_1 1))
                                                                                                                                                                                                                                                            (set! value_reg v)
                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                            (set! pc apply-cont))
                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-62>)
                                                                                                                                                                                                                                                              (let ((fexps 'undefined)
                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                (set! fexps (list-ref temp_1 1))
                                                                                                                                                                                                                                                                (set! k_reg (make-cont '<cont-61> value_reg k))
                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                (set! exps_reg fexps)
                                                                                                                                                                                                                                                                (set! pc eval-sequence))
                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-63>)
                                                                                                                                                                                                                                                                  (let ((clauses 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                                                                    (set! clauses (list-ref temp_1 1))
                                                                                                                                                                                                                                                                    (set-binding-value! value_reg (make-pattern-macro clauses))
                                                                                                                                                                                                                                                                    (set! value_reg '<void>)
                                                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-64>)
                                                                                                                                                                                                                                                                      (let ((docstring 'undefined)
                                                                                                                                                                                                                                                                            (rhs-value 'undefined)
                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                        (set! rhs-value (list-ref temp_1 2))
                                                                                                                                                                                                                                                                        (set! docstring (list-ref temp_1 1))
                                                                                                                                                                                                                                                                        (set-binding-docstring! value_reg docstring)
                                                                                                                                                                                                                                                                        (set-binding-value! value_reg rhs-value)
                                                                                                                                                                                                                                                                        (set! value_reg '<void>)
                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-65>)
                                                                                                                                                                                                                                                                          (let ((rhs-value 'undefined)
                                                                                                                                                                                                                                                                                (var 'undefined)
                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                            (set! var (list-ref temp_1 2))
                                                                                                                                                                                                                                                                            (set! rhs-value (list-ref temp_1 1))
                                                                                                                                                                                                                                                                            (set! k_reg (make-cont '<cont-64> value_reg rhs-value k))
                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                            (set! var_reg var)
                                                                                                                                                                                                                                                                            (set! pc lookup-binding-in-first-frame))
                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-66>)
                                                                                                                                                                                                                                                                              (let ((rhs-exp 'undefined)
                                                                                                                                                                                                                                                                                    (var 'undefined)
                                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                (set! var (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                (set! rhs-exp (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                (set! k_reg
                                                                                                                                                                                                                                                                                  (make-cont '<cont-65> value_reg var env handler k))
                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                                (set! exp_reg (car rhs-exp))
                                                                                                                                                                                                                                                                                (set! pc m))
                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-67>)
                                                                                                                                                                                                                                                                                  (let ((rhs-value 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                    (set! rhs-value (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                    (set-binding-value! value_reg rhs-value)
                                                                                                                                                                                                                                                                                    (set! value_reg '<void>)
                                                                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-68>)
                                                                                                                                                                                                                                                                                      (let ((var 'undefined)
                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                        (set! var (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                        (set! k_reg (make-cont '<cont-67> value_reg k))
                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                        (set! var_reg var)
                                                                                                                                                                                                                                                                                        (set! pc lookup-binding-in-first-frame))
                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-69>)
                                                                                                                                                                                                                                                                                          (let ((var 'undefined)
                                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                            (set! var (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                            (set! k_reg (make-cont '<cont-67> value_reg k))
                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                                            (set! variable_reg var)
                                                                                                                                                                                                                                                                                            (set! pc lookup-binding))
                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-70>)
                                                                                                                                                                                                                                                                                              (let ((else-exp 'undefined)
                                                                                                                                                                                                                                                                                                    (then-exp 'undefined)
                                                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                (set! then-exp (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                (set! else-exp (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                (if value_reg
                                                                                                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                      (set! env_reg env)
                                                                                                                                                                                                                                                                                                      (set! exp_reg then-exp)
                                                                                                                                                                                                                                                                                                      (set! pc m))
                                                                                                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                      (set! env_reg env)
                                                                                                                                                                                                                                                                                                      (set! exp_reg else-exp)
                                                                                                                                                                                                                                                                                                      (set! pc m))))
                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-71>)
                                                                                                                                                                                                                                                                                                  (let ((e 'undefined) (handler 'undefined))
                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                    (set! e (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                    (set! exception_reg e)
                                                                                                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                    (set! pc apply-handler))
                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-72>)
                                                                                                                                                                                                                                                                                                      (let ((exps 'undefined)
                                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                        (set! exps (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                        (set! k_reg (make-cont '<cont-47> value_reg k))
                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                                        (set! exps_reg (cdr exps))
                                                                                                                                                                                                                                                                                                        (set! pc m*))
                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-73>)
                                                                                                                                                                                                                                                                                                          (let ((exps 'undefined)
                                                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                            (set! exps (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                            (if (null? (cdr exps))
                                                                                                                                                                                                                                                                                                                (begin (set! k_reg k) (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                (begin
                                                                                                                                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                                                                                                                                  (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                  (set! env_reg env)
                                                                                                                                                                                                                                                                                                                  (set! exps_reg (cdr exps))
                                                                                                                                                                                                                                                                                                                  (set! pc eval-sequence))))
                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-74>)
                                                                                                                                                                                                                                                                                                              (let ((handler 'undefined) (k2 'undefined))
                                                                                                                                                                                                                                                                                                                (set! k2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                (set! k_reg k2)
                                                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                (set! env_reg toplevel-env)
                                                                                                                                                                                                                                                                                                                (set! exp_reg value_reg)
                                                                                                                                                                                                                                                                                                                (set! pc m))
                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-75>)
                                                                                                                                                                                                                                                                                                                  (let ((list1 'undefined)
                                                                                                                                                                                                                                                                                                                        (proc 'undefined)
                                                                                                                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                    (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                    (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                    (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                    (set! k_reg (make-cont '<cont-47> value_reg k))
                                                                                                                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                    (set! env_reg env)
                                                                                                                                                                                                                                                                                                                    (set! list1_reg (cdr list1))
                                                                                                                                                                                                                                                                                                                    (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                    (set! pc map1))
                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-76>)
                                                                                                                                                                                                                                                                                                                      (let ((list1 'undefined) (proc 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                        (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                        (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                        (set! value_reg
                                                                                                                                                                                                                                                                                                                          (cons (dlr-apply proc (list (car list1))) value_reg))
                                                                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-77>)
                                                                                                                                                                                                                                                                                                                          (let ((list1 'undefined)
                                                                                                                                                                                                                                                                                                                                (list2 'undefined)
                                                                                                                                                                                                                                                                                                                                (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 6))
                                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                            (set! proc (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                            (set! list2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                            (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                            (set! k_reg (make-cont '<cont-47> value_reg k))
                                                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                                                                            (set! list2_reg (cdr list2))
                                                                                                                                                                                                                                                                                                                            (set! list1_reg (cdr list1))
                                                                                                                                                                                                                                                                                                                            (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                            (set! pc map2))
                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-78>)
                                                                                                                                                                                                                                                                                                                              (let ((list1 'undefined)
                                                                                                                                                                                                                                                                                                                                    (list2 'undefined)
                                                                                                                                                                                                                                                                                                                                    (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                (set! proc (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                (set! list2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                (set! value_reg
                                                                                                                                                                                                                                                                                                                                  (cons
                                                                                                                                                                                                                                                                                                                                    (dlr-apply proc (list (car list1) (car list2)))
                                                                                                                                                                                                                                                                                                                                    value_reg))
                                                                                                                                                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-79>)
                                                                                                                                                                                                                                                                                                                                  (let ((lists 'undefined)
                                                                                                                                                                                                                                                                                                                                        (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                    (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                    (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                    (set! lists (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                    (set! k_reg (make-cont '<cont-47> value_reg k))
                                                                                                                                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                    (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                    (set! lists_reg (map cdr lists))
                                                                                                                                                                                                                                                                                                                                    (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                    (set! pc mapN))
                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-80>)
                                                                                                                                                                                                                                                                                                                                      (let ((lists 'undefined) (proc 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                        (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                        (set! lists (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                        (set! value_reg
                                                                                                                                                                                                                                                                                                                                          (cons (dlr-apply proc (map car lists)) value_reg))
                                                                                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-81>)
                                                                                                                                                                                                                                                                                                                                          (let ((arg-list 'undefined)
                                                                                                                                                                                                                                                                                                                                                (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                            (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                            (set! arg-list (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                            (set! lists_reg (map cdr arg-list))
                                                                                                                                                                                                                                                                                                                                            (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                            (set! pc for-each-prim))
                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-82>)
                                                                                                                                                                                                                                                                                                                                              (let ((args 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (sym 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                (set! sym (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                (set! args (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                (if (null? (cdr args))
                                                                                                                                                                                                                                                                                                                                                    (begin (set! k_reg k) (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                                                    (if (not (environment? value_reg))
                                                                                                                                                                                                                                                                                                                                                        (begin
                                                                                                                                                                                                                                                                                                                                                          (set! exception_reg (format "~a is not a module" sym))
                                                                                                                                                                                                                                                                                                                                                          (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                          (set! pc apply-handler))
                                                                                                                                                                                                                                                                                                                                                        (begin
                                                                                                                                                                                                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                          (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                          (set! env_reg value_reg)
                                                                                                                                                                                                                                                                                                                                                          (set! args_reg (cdr args))
                                                                                                                                                                                                                                                                                                                                                          (set! pc get-primitive)))))
                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-83>)
                                                                                                                                                                                                                                                                                                                                                  (let ((filename 'undefined)
                                                                                                                                                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                    (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                    (set! filename (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                    (let ((module 'undefined))
                                                                                                                                                                                                                                                                                                                                                      (set! module (extend env '() '()))
                                                                                                                                                                                                                                                                                                                                                      (set-binding-value! value_reg module)
                                                                                                                                                                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                      (set! env_reg module)
                                                                                                                                                                                                                                                                                                                                                      (set! filename_reg filename)
                                                                                                                                                                                                                                                                                                                                                      (set! pc load-file)))
                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-84>)
                                                                                                                                                                                                                                                                                                                                                      (let ((k 'undefined))
                                                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                        (set! load-stack (cdr load-stack))
                                                                                                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-85>)
                                                                                                                                                                                                                                                                                                                                                          (let ((env 'undefined)
                                                                                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                            (set! k_reg (make-cont '<cont-84> k))
                                                                                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                            (set! tokens_reg value_reg)
                                                                                                                                                                                                                                                                                                                                                            (set! pc load-loop))
                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-86>)
                                                                                                                                                                                                                                                                                                                                                              (let ((tokens-left 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                (set! tokens-left (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                                (set! tokens_reg tokens-left)
                                                                                                                                                                                                                                                                                                                                                                (set! pc load-loop))
                                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-87>)
                                                                                                                                                                                                                                                                                                                                                                  (let ((tokens-left 'undefined)
                                                                                                                                                                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                    (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                    (set! tokens-left (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                    (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                      (make-cont '<cont-86> tokens-left env
                                                                                                                                                                                                                                                                                                                                                                        handler k))
                                                                                                                                                                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                                    (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                                    (set! exp_reg value_reg)
                                                                                                                                                                                                                                                                                                                                                                    (set! pc m))
                                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-88>)
                                                                                                                                                                                                                                                                                                                                                                      (let ((filenames 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                        (set! filenames (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                                        (set! filenames_reg (cdr filenames))
                                                                                                                                                                                                                                                                                                                                                                        (set! pc load-files))
                                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-89>)
                                                                                                                                                                                                                                                                                                                                                                          (let ((k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                            (set! value_reg
                                                                                                                                                                                                                                                                                                                                                                              (binding-docstring value_reg))
                                                                                                                                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                            (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-90>)
                                                                                                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                                                                                                (set! k_reg init-cont)
                                                                                                                                                                                                                                                                                                                                                                                (set! handler_reg init-handler)
                                                                                                                                                                                                                                                                                                                                                                                (set! env_reg toplevel-env)
                                                                                                                                                                                                                                                                                                                                                                                (set! exp_reg value_reg)
                                                                                                                                                                                                                                                                                                                                                                                (set! pc m))
                                                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-91>)
                                                                                                                                                                                                                                                                                                                                                                                  (let ((pattern 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                        (var 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                                    (set! var
                                                                                                                                                                                                                                                                                                                                                                                      (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                                    (set! pattern
                                                                                                                                                                                                                                                                                                                                                                                      (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                                    (if value_reg
                                                                                                                                                                                                                                                                                                                                                                                        (begin
                                                                                                                                                                                                                                                                                                                                                                                          (set! value_reg #t)
                                                                                                                                                                                                                                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                                          (set! pc apply-cont))
                                                                                                                                                                                                                                                                                                                                                                                        (begin
                                                                                                                                                                                                                                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                                          (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                            (cdr pattern))
                                                                                                                                                                                                                                                                                                                                                                                          (set! var_reg var)
                                                                                                                                                                                                                                                                                                                                                                                          (set! pc occurs?))))
                                                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                           '<cont-92>)
                                                                                                                                                                                                                                                                                                                                                                                      (let ((p1 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                            (p2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                        (set! k
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                                        (set! p2
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                                        (set! p1
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                                        (if value_reg
                                                                                                                                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                                                                                                                                              (set! value_reg
                                                                                                                                                                                                                                                                                                                                                                                                #f)
                                                                                                                                                                                                                                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                                              (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                apply-cont))
                                                                                                                                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                                                                                                                                              (set! value_reg
                                                                                                                                                                                                                                                                                                                                                                                                (make-sub
                                                                                                                                                                                                                                                                                                                                                                                                  'unit
                                                                                                                                                                                                                                                                                                                                                                                                  p1
                                                                                                                                                                                                                                                                                                                                                                                                  p2))
                                                                                                                                                                                                                                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                                              (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                apply-cont))))
                                                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                               '<cont-93>)
                                                                                                                                                                                                                                                                                                                                                                                          (let ((s-car 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                            (set! k
                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                2))
                                                                                                                                                                                                                                                                                                                                                                                            (set! s-car
                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                1))
                                                                                                                                                                                                                                                                                                                                                                                            (if (not value_reg)
                                                                                                                                                                                                                                                                                                                                                                                                (begin
                                                                                                                                                                                                                                                                                                                                                                                                  (set! value_reg
                                                                                                                                                                                                                                                                                                                                                                                                    #f)
                                                                                                                                                                                                                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                                                  (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                    apply-cont))
                                                                                                                                                                                                                                                                                                                                                                                                (begin
                                                                                                                                                                                                                                                                                                                                                                                                  (set! value_reg
                                                                                                                                                                                                                                                                                                                                                                                                    (make-sub
                                                                                                                                                                                                                                                                                                                                                                                                      'composite
                                                                                                                                                                                                                                                                                                                                                                                                      s-car
                                                                                                                                                                                                                                                                                                                                                                                                      value_reg))
                                                                                                                                                                                                                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                                                  (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                    apply-cont))))
                                                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                                   '<cont-94>)
                                                                                                                                                                                                                                                                                                                                                                                              (let ((new-cdr1 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                    (s-car 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                                (set! k
                                                                                                                                                                                                                                                                                                                                                                                                  (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                    temp_1
                                                                                                                                                                                                                                                                                                                                                                                                    3))
                                                                                                                                                                                                                                                                                                                                                                                                (set! s-car
                                                                                                                                                                                                                                                                                                                                                                                                  (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                    temp_1
                                                                                                                                                                                                                                                                                                                                                                                                    2))
                                                                                                                                                                                                                                                                                                                                                                                                (set! new-cdr1
                                                                                                                                                                                                                                                                                                                                                                                                  (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                    temp_1
                                                                                                                                                                                                                                                                                                                                                                                                    1))
                                                                                                                                                                                                                                                                                                                                                                                                (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                                                  (make-cont
                                                                                                                                                                                                                                                                                                                                                                                                    '<cont-93>
                                                                                                                                                                                                                                                                                                                                                                                                    s-car
                                                                                                                                                                                                                                                                                                                                                                                                    k))
                                                                                                                                                                                                                                                                                                                                                                                                (set! p2_reg
                                                                                                                                                                                                                                                                                                                                                                                                  value_reg)
                                                                                                                                                                                                                                                                                                                                                                                                (set! p1_reg
                                                                                                                                                                                                                                                                                                                                                                                                  new-cdr1)
                                                                                                                                                                                                                                                                                                                                                                                                (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                  unify-patterns))
                                                                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                                       '<cont-95>)
                                                                                                                                                                                                                                                                                                                                                                                                  (let ((pair2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                        (s-car 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                                    (set! k
                                                                                                                                                                                                                                                                                                                                                                                                      (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                        temp_1
                                                                                                                                                                                                                                                                                                                                                                                                        3))
                                                                                                                                                                                                                                                                                                                                                                                                    (set! s-car
                                                                                                                                                                                                                                                                                                                                                                                                      (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                        temp_1
                                                                                                                                                                                                                                                                                                                                                                                                        2))
                                                                                                                                                                                                                                                                                                                                                                                                    (set! pair2
                                                                                                                                                                                                                                                                                                                                                                                                      (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                        temp_1
                                                                                                                                                                                                                                                                                                                                                                                                        1))
                                                                                                                                                                                                                                                                                                                                                                                                    (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                                                      (make-cont
                                                                                                                                                                                                                                                                                                                                                                                                        '<cont-94>
                                                                                                                                                                                                                                                                                                                                                                                                        value_reg
                                                                                                                                                                                                                                                                                                                                                                                                        s-car
                                                                                                                                                                                                                                                                                                                                                                                                        k))
                                                                                                                                                                                                                                                                                                                                                                                                    (set! s_reg
                                                                                                                                                                                                                                                                                                                                                                                                      s-car)
                                                                                                                                                                                                                                                                                                                                                                                                    (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                                      (cdr pair2))
                                                                                                                                                                                                                                                                                                                                                                                                    (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                      instantiate))
                                                                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                                           '<cont-96>)
                                                                                                                                                                                                                                                                                                                                                                                                      (let ((pair1 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                            (pair2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                                        (set! k
                                                                                                                                                                                                                                                                                                                                                                                                          (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                            temp_1
                                                                                                                                                                                                                                                                                                                                                                                                            3))
                                                                                                                                                                                                                                                                                                                                                                                                        (set! pair2
                                                                                                                                                                                                                                                                                                                                                                                                          (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                            temp_1
                                                                                                                                                                                                                                                                                                                                                                                                            2))
                                                                                                                                                                                                                                                                                                                                                                                                        (set! pair1
                                                                                                                                                                                                                                                                                                                                                                                                          (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                            temp_1
                                                                                                                                                                                                                                                                                                                                                                                                            1))
                                                                                                                                                                                                                                                                                                                                                                                                        (if (not value_reg)
                                                                                                                                                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                                                                                                                                                              (set! value_reg
                                                                                                                                                                                                                                                                                                                                                                                                                #f)
                                                                                                                                                                                                                                                                                                                                                                                                              (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                                                                k)
                                                                                                                                                                                                                                                                                                                                                                                                              (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                                apply-cont))
                                                                                                                                                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                                                                                                                                                              (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                                                                (make-cont
                                                                                                                                                                                                                                                                                                                                                                                                                  '<cont-95>
                                                                                                                                                                                                                                                                                                                                                                                                                  pair2
                                                                                                                                                                                                                                                                                                                                                                                                                  value_reg
                                                                                                                                                                                                                                                                                                                                                                                                                  k))
                                                                                                                                                                                                                                                                                                                                                                                                              (set! s_reg
                                                                                                                                                                                                                                                                                                                                                                                                                value_reg)
                                                                                                                                                                                                                                                                                                                                                                                                              (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                                                (cdr pair1))
                                                                                                                                                                                                                                                                                                                                                                                                              (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                                instantiate))))
                                                                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                                               '<cont-97>)
                                                                                                                                                                                                                                                                                                                                                                                                          (let ((pattern 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                                (s 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                                            (set! k
                                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                                3))
                                                                                                                                                                                                                                                                                                                                                                                                            (set! s
                                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                                2))
                                                                                                                                                                                                                                                                                                                                                                                                            (set! pattern
                                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                                1))
                                                                                                                                                                                                                                                                                                                                                                                                            (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                                                              (make-cont
                                                                                                                                                                                                                                                                                                                                                                                                                '<cont-40>
                                                                                                                                                                                                                                                                                                                                                                                                                value_reg
                                                                                                                                                                                                                                                                                                                                                                                                                k))
                                                                                                                                                                                                                                                                                                                                                                                                            (set! s_reg
                                                                                                                                                                                                                                                                                                                                                                                                              s)
                                                                                                                                                                                                                                                                                                                                                                                                            (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                                              (cdr pattern))
                                                                                                                                                                                                                                                                                                                                                                                                            (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                              instantiate))
                                                                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                                                   '<cont-98>)
                                                                                                                                                                                                                                                                                                                                                                                                              (let ((s2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                                                (set! k
                                                                                                                                                                                                                                                                                                                                                                                                                  (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                                    temp_1
                                                                                                                                                                                                                                                                                                                                                                                                                    2))
                                                                                                                                                                                                                                                                                                                                                                                                                (set! s2
                                                                                                                                                                                                                                                                                                                                                                                                                  (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                                    temp_1
                                                                                                                                                                                                                                                                                                                                                                                                                    1))
                                                                                                                                                                                                                                                                                                                                                                                                                (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                                                                  k)
                                                                                                                                                                                                                                                                                                                                                                                                                (set! s_reg
                                                                                                                                                                                                                                                                                                                                                                                                                  s2)
                                                                                                                                                                                                                                                                                                                                                                                                                (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                                                  value_reg)
                                                                                                                                                                                                                                                                                                                                                                                                                (set! pc
                                                                                                                                                                                                                                                                                                                                                                                                                  instantiate))
                                                                                                                                                                                                                                                                                                                                                                                                              (error 'apply-cont
                                                                                                                                                                                                                                                                                                                                                                                                                "bad continuation: ~a"
                                                                                                                                                                                                                                                                                                                                                                                                                k_reg))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define make-cont2
  (lambda args (return* (cons 'continuation2 args))))

(define*
  apply-cont2
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr k_reg))
      (if (eq? (car temp_1) '<cont2-1>)
          (let ((handler 'undefined) (k 'undefined))
            (set! k (list-ref temp_1 2))
            (set! handler (list-ref temp_1 1))
            (if (token-type? value1_reg 'end-marker)
                (begin
                  (set! value_reg (list value1_reg))
                  (set! k_reg k)
                  (set! pc apply-cont))
                (begin
                  (set! k_reg (make-cont '<cont-1> value1_reg k))
                  (set! handler_reg handler)
                  (set! chars_reg value2_reg)
                  (set! pc scan-input-loop))))
          (if (eq? (car temp_1) '<cont2-2>)
              (begin (set! final_reg value1_reg) (set! pc #f))
              (if (eq? (car temp_1) '<cont2-3>)
                  (let ((handler 'undefined) (k 'undefined))
                    (set! k (list-ref temp_1 2))
                    (set! handler (list-ref temp_1 1))
                    (if (token-type? (first value2_reg) 'end-marker)
                        (begin (set! k_reg k) (set! pc apply-cont2))
                        (begin
                          (set! exception_reg
                            (format
                              "tokens left over at line: ~a col: ~a"
                              (get-line-count (first value2_reg))
                              (get-char-count (first value2_reg))))
                          (set! handler_reg handler)
                          (set! pc apply-handler))))
                  (if (eq? (car temp_1) '<cont2-4>)
                      (let ((k 'undefined))
                        (set! k (list-ref temp_1 1))
                        (set! value1_reg (list->vector value1_reg))
                        (set! k_reg k)
                        (set! pc apply-cont2))
                      (if (eq? (car temp_1) '<cont2-5>)
                          (let ((keyword 'undefined) (k 'undefined))
                            (set! k (list-ref temp_1 2))
                            (set! keyword (list-ref temp_1 1))
                            (set! value1_reg (list keyword value1_reg))
                            (set! k_reg k)
                            (set! pc apply-cont2))
                          (if (eq? (car temp_1) '<cont2-6>)
                              (let ((sexp1 'undefined) (k 'undefined))
                                (set! k (list-ref temp_1 2))
                                (set! sexp1 (list-ref temp_1 1))
                                (set! value1_reg (cons sexp1 value1_reg))
                                (set! k_reg k)
                                (set! pc apply-cont2))
                              (if (eq? (car temp_1) '<cont2-7>)
                                  (let ((expected-terminator 'undefined)
                                        (handler 'undefined)
                                        (k 'undefined))
                                    (set! k (list-ref temp_1 3))
                                    (set! handler (list-ref temp_1 2))
                                    (set! expected-terminator (list-ref temp_1 1))
                                    (set! k_reg (make-cont2 '<cont2-6> value1_reg k))
                                    (set! handler_reg handler)
                                    (set! expected-terminator_reg expected-terminator)
                                    (set! tokens_reg value2_reg)
                                    (set! pc read-sexp-sequence))
                                  (if (eq? (car temp_1) '<cont2-8>)
                                      (let ((expected-terminator 'undefined)
                                            (handler 'undefined)
                                            (k 'undefined))
                                        (set! k (list-ref temp_1 3))
                                        (set! handler (list-ref temp_1 2))
                                        (set! expected-terminator (list-ref temp_1 1))
                                        (set! k_reg k)
                                        (set! handler_reg handler)
                                        (set! expected-terminator_reg expected-terminator)
                                        (set! tokens_reg value2_reg)
                                        (set! sexp_reg value1_reg)
                                        (set! pc close-sexp-sequence))
                                      (if (eq? (car temp_1) '<cont2-9>)
                                          (let ((handler 'undefined) (k 'undefined))
                                            (set! k (list-ref temp_1 2))
                                            (set! handler (list-ref temp_1 1))
                                            (set! k_reg (make-cont2 '<cont2-6> value1_reg k))
                                            (set! handler_reg handler)
                                            (set! tokens_reg value2_reg)
                                            (set! pc read-vector))
                                          (if (eq? (car temp_1) '<cont2-10>)
                                              (let ((handler 'undefined) (k 'undefined))
                                                (set! k (list-ref temp_1 2))
                                                (set! handler (list-ref temp_1 1))
                                                (pretty-print value1_reg)
                                                (set! k_reg k)
                                                (set! handler_reg handler)
                                                (set! tokens_reg value2_reg)
                                                (set! pc print-unparsed-sexps))
                                              (if (eq? (car temp_1) '<cont2-11>)
                                                  (begin
                                                    (set! final_reg (cons value1_reg value2_reg))
                                                    (set! pc #f))
                                                  (if (eq? (car temp_1) '<cont2-12>)
                                                      (let ((bodies 'undefined) (k 'undefined))
                                                        (set! k (list-ref temp_1 2))
                                                        (set! bodies (list-ref temp_1 1))
                                                        (set! value_reg
                                                          (cons 'let (cons value1_reg (append value2_reg bodies))))
                                                        (set! k_reg k)
                                                        (set! pc apply-cont))
                                                      (if (eq? (car temp_1) '<cont2-13>)
                                                          (let ((procs 'undefined) (vars 'undefined) (k2 'undefined))
                                                            (set! k2 (list-ref temp_1 3))
                                                            (set! vars (list-ref temp_1 2))
                                                            (set! procs (list-ref temp_1 1))
                                                            (set! value2_reg
                                                              (cons (list 'set! (car vars) (car procs)) value2_reg))
                                                            (set! value1_reg
                                                              (cons
                                                                (list (car vars) (list 'quote 'undefined))
                                                                value1_reg))
                                                            (set! k_reg k2)
                                                            (set! pc apply-cont2))
                                                          (if (eq? (car temp_1) '<cont2-14>)
                                                              (let ((exp 'undefined) (k 'undefined))
                                                                (set! k (list-ref temp_1 2))
                                                                (set! exp (list-ref temp_1 1))
                                                                (set! value_reg
                                                                  (list
                                                                    'let
                                                                    (cons (list 'r exp) value1_reg)
                                                                    (cons 'cond value2_reg)))
                                                                (set! k_reg k)
                                                                (set! pc apply-cont))
                                                              (if (eq? (car temp_1) '<cont2-15>)
                                                                  (let ((clauses 'undefined) (var 'undefined) (k2 'undefined))
                                                                    (set! k2 (list-ref temp_1 3))
                                                                    (set! var (list-ref temp_1 2))
                                                                    (set! clauses (list-ref temp_1 1))
                                                                    (let ((clause 'undefined))
                                                                      (set! clause (car clauses))
                                                                      (if (eq? (car clause) 'else)
                                                                          (begin
                                                                            (set! value2_reg
                                                                              (cons (list 'else (list 'else-code)) value2_reg))
                                                                            (set! value1_reg
                                                                              (cons
                                                                                (list 'else-code (cons 'lambda (cons '() (cdr clause))))
                                                                                value1_reg))
                                                                            (set! k_reg k2)
                                                                            (set! pc apply-cont2))
                                                                          (if (symbol? (car clause))
                                                                              (let ((name 'undefined))
                                                                                (set! name (car clause))
                                                                                (set! value2_reg
                                                                                  (cons
                                                                                    (list
                                                                                      (list 'eq? var (list 'quote (car clause)))
                                                                                      (list name))
                                                                                    value2_reg))
                                                                                (set! value1_reg
                                                                                  (cons
                                                                                    (list name (cons 'lambda (cons '() (cdr clause))))
                                                                                    value1_reg))
                                                                                (set! k_reg k2)
                                                                                (set! pc apply-cont2))
                                                                              (let ((name 'undefined))
                                                                                (set! name (caar clause))
                                                                                (set! value2_reg
                                                                                  (cons
                                                                                    (list
                                                                                      (list 'memq var (list 'quote (car clause)))
                                                                                      (list name))
                                                                                    value2_reg))
                                                                                (set! value1_reg
                                                                                  (cons
                                                                                    (list name (cons 'lambda (cons '() (cdr clause))))
                                                                                    value1_reg))
                                                                                (set! k_reg k2)
                                                                                (set! pc apply-cont2))))))
                                                                  (if (eq? (car temp_1) '<cont2-16>)
                                                                      (let ((k 'undefined))
                                                                        (set! k (list-ref temp_1 1))
                                                                        (set! value_reg
                                                                          (list 'let value1_reg (cons 'cond value2_reg)))
                                                                        (set! k_reg k)
                                                                        (set! pc apply-cont))
                                                                      (if (eq? (car temp_1) '<cont2-17>)
                                                                          (let ((clauses 'undefined) (var 'undefined) (k2 'undefined))
                                                                            (set! k2 (list-ref temp_1 3))
                                                                            (set! var (list-ref temp_1 2))
                                                                            (set! clauses (list-ref temp_1 1))
                                                                            (let ((clause 'undefined))
                                                                              (set! clause (car clauses))
                                                                              (if (eq? (car clause) 'else)
                                                                                  (begin
                                                                                    (set! value2_reg
                                                                                      (cons (list 'else (list 'else-code)) value2_reg))
                                                                                    (set! value1_reg
                                                                                      (cons
                                                                                        (list 'else-code (cons 'lambda (cons '() (cdr clause))))
                                                                                        value1_reg))
                                                                                    (set! k_reg k2)
                                                                                    (set! pc apply-cont2))
                                                                                  (if (symbol? (car clause))
                                                                                      (let ((name 'undefined))
                                                                                        (set! name (car clause))
                                                                                        (set! value2_reg
                                                                                          (cons
                                                                                            (list
                                                                                              (list 'eq? (list 'car var) (list 'quote (car clause)))
                                                                                              (list 'apply name (list 'cdr var)))
                                                                                            value2_reg))
                                                                                        (set! value1_reg
                                                                                          (cons
                                                                                            (list
                                                                                              name
                                                                                              (cons 'lambda (cons (cadr clause) (cddr clause))))
                                                                                            value1_reg))
                                                                                        (set! k_reg k2)
                                                                                        (set! pc apply-cont2))
                                                                                      (let ((name 'undefined))
                                                                                        (set! name (caar clause))
                                                                                        (set! value2_reg
                                                                                          (cons
                                                                                            (list
                                                                                              (list 'memq (list 'car var) (list 'quote (car clause)))
                                                                                              (list 'apply name (list 'cdr var)))
                                                                                            value2_reg))
                                                                                        (set! value1_reg
                                                                                          (cons
                                                                                            (list
                                                                                              name
                                                                                              (cons 'lambda (cons (cadr clause) (cddr clause))))
                                                                                            value1_reg))
                                                                                        (set! k_reg k2)
                                                                                        (set! pc apply-cont2))))))
                                                                          (if (eq? (car temp_1) '<cont2-18>)
                                                                              (begin
                                                                                (set! k_reg init-cont)
                                                                                (set! handler_reg init-handler)
                                                                                (set! datum_reg value1_reg)
                                                                                (set! pc parse))
                                                                              (if (eq? (car temp_1) '<cont2-19>)
                                                                                  (let ((handler 'undefined) (k 'undefined))
                                                                                    (set! k (list-ref temp_1 2))
                                                                                    (set! handler (list-ref temp_1 1))
                                                                                    (set! k_reg (make-cont '<cont-51> value2_reg handler k))
                                                                                    (set! handler_reg handler)
                                                                                    (set! datum_reg value1_reg)
                                                                                    (set! pc parse))
                                                                                  (if (eq? (car temp_1) '<cont2-20>)
                                                                                      (begin
                                                                                        (set! k_reg (make-cont '<cont-52>))
                                                                                        (set! handler_reg REP-handler)
                                                                                        (set! datum_reg value1_reg)
                                                                                        (set! pc parse))
                                                                                      (if (eq? (car temp_1) '<cont2-21>)
                                                                                          (begin
                                                                                            (set! k_reg (make-cont '<cont-54>))
                                                                                            (set! handler_reg REP-handler)
                                                                                            (set! datum_reg value1_reg)
                                                                                            (set! pc parse))
                                                                                          (if (eq? (car temp_1) '<cont2-22>)
                                                                                              (begin
                                                                                                (set! k_reg (make-cont '<cont-56>))
                                                                                                (set! handler_reg REP-handler)
                                                                                                (set! datum_reg value1_reg)
                                                                                                (set! pc parse))
                                                                                              (if (eq? (car temp_1) '<cont2-23>)
                                                                                                  (let ((handler 'undefined) (k2 'undefined))
                                                                                                    (set! k2 (list-ref temp_1 2))
                                                                                                    (set! handler (list-ref temp_1 1))
                                                                                                    (set! k_reg k2)
                                                                                                    (set! handler_reg handler)
                                                                                                    (set! datum_reg value1_reg)
                                                                                                    (set! pc parse))
                                                                                                  (if (eq? (car temp_1) '<cont2-24>)
                                                                                                      (let ((env 'undefined) (handler 'undefined) (k 'undefined))
                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                        (set! handler (list-ref temp_1 2))
                                                                                                        (set! env (list-ref temp_1 1))
                                                                                                        (set! k_reg (make-cont '<cont-87> value2_reg env handler k))
                                                                                                        (set! handler_reg handler)
                                                                                                        (set! datum_reg value1_reg)
                                                                                                        (set! pc parse))
                                                                                                      (if (eq? (car temp_1) '<cont2-25>)
                                                                                                          (begin
                                                                                                            (set! k_reg (make-cont '<cont-90>))
                                                                                                            (set! handler_reg init-handler)
                                                                                                            (set! datum_reg value1_reg)
                                                                                                            (set! pc parse))
                                                                                                          (error 'apply-cont2
                                                                                                            "bad continuation2: ~a"
                                                                                                            k_reg)))))))))))))))))))))))))))))

(define make-handler
  (lambda args (return* (cons 'handler args))))

(define*
  apply-handler
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr handler_reg))
      (if (eq? (car temp_1) '<handler-1>)
          (begin
            (set! final_reg (list 'exception exception_reg))
            (set! pc #f))
          (if (eq? (car temp_1) '<handler-2>)
              (begin
                (set! value_reg (list 'uncaught 'exception: exception_reg))
                (set! k_reg REP-k)
                (set! pc apply-cont))
              (if (eq? (car temp_1) '<handler-3>)
                  (let ((cexps 'undefined)
                        (cvar 'undefined)
                        (env 'undefined)
                        (handler 'undefined)
                        (k 'undefined))
                    (set! k (list-ref temp_1 5))
                    (set! handler (list-ref temp_1 4))
                    (set! env (list-ref temp_1 3))
                    (set! cvar (list-ref temp_1 2))
                    (set! cexps (list-ref temp_1 1))
                    (let ((new-env 'undefined))
                      (set! new-env (extend env (list cvar) (list exception_reg)))
                      (set! k_reg k)
                      (set! handler_reg handler)
                      (set! env_reg new-env)
                      (set! exps_reg cexps)
                      (set! pc eval-sequence)))
                  (if (eq? (car temp_1) '<handler-4>)
                      (let ((fexps 'undefined)
                            (env 'undefined)
                            (handler 'undefined))
                        (set! handler (list-ref temp_1 3))
                        (set! env (list-ref temp_1 2))
                        (set! fexps (list-ref temp_1 1))
                        (set! k_reg (make-cont '<cont-71> exception_reg handler))
                        (set! handler_reg handler)
                        (set! env_reg env)
                        (set! exps_reg fexps)
                        (set! pc eval-sequence))
                      (if (eq? (car temp_1) '<handler-5>)
                          (let ((cexps 'undefined)
                                (cvar 'undefined)
                                (fexps 'undefined)
                                (env 'undefined)
                                (handler 'undefined)
                                (k 'undefined))
                            (set! k (list-ref temp_1 6))
                            (set! handler (list-ref temp_1 5))
                            (set! env (list-ref temp_1 4))
                            (set! fexps (list-ref temp_1 3))
                            (set! cvar (list-ref temp_1 2))
                            (set! cexps (list-ref temp_1 1))
                            (let ((new-env 'undefined))
                              (set! new-env (extend env (list cvar) (list exception_reg)))
                              (let ((catch-handler 'undefined))
                                (set! catch-handler (try-finally-handler fexps env handler))
                                (set! k_reg (make-cont '<cont-62> fexps env handler k))
                                (set! handler_reg catch-handler)
                                (set! env_reg new-env)
                                (set! exps_reg cexps)
                                (set! pc eval-sequence))))
                          (error 'apply-handler "bad handler: ~a" handler_reg)))))))))

(define make-proc
  (lambda args (return* (cons 'procedure args))))

(define*
  apply-proc
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr proc_reg))
      (if (eq? (car temp_1) '<proc-1>)
          (let ((formals 'undefined)
                (body 'undefined)
                (env 'undefined))
            (set! env (list-ref temp_1 3))
            (set! body (list-ref temp_1 2))
            (set! formals (list-ref temp_1 1))
            (if (= (length args_reg) (length formals))
                (begin
                  (set! k_reg k2_reg)
                  (set! env_reg (extend env formals args_reg))
                  (set! exp_reg body)
                  (set! pc m))
                (begin
                  (set! exception_reg "incorrect number of arguments")
                  (set! pc apply-handler))))
          (if (eq? (car temp_1) '<proc-2>)
              (let ((formals 'undefined)
                    (runt 'undefined)
                    (body 'undefined)
                    (env 'undefined))
                (set! env (list-ref temp_1 4))
                (set! body (list-ref temp_1 3))
                (set! runt (list-ref temp_1 2))
                (set! formals (list-ref temp_1 1))
                (if (>= (length args_reg) (length formals))
                    (let ((new-env 'undefined))
                      (set! new-env
                        (extend
                          env
                          (cons runt formals)
                          (cons
                            (list-tail args_reg (length formals))
                            (list-head args_reg (length formals)))))
                      (set! k_reg k2_reg)
                      (set! env_reg new-env)
                      (set! exp_reg body)
                      (set! pc m))
                    (begin
                      (set! exception_reg "not enough arguments given")
                      (set! pc apply-handler))))
              (if (eq? (car temp_1) '<proc-3>)
                  (begin
                    (set! k_reg k2_reg)
                    (set! env_reg env2_reg)
                    (set! var_reg (car args_reg))
                    (set! pc help-prim))
                  (if (eq? (car temp_1) '<proc-4>)
                      (begin
                        (set! value_reg (make-vector-size (car args_reg)))
                        (set! k_reg k2_reg)
                        (set! pc apply-cont))
                      (if (eq? (car temp_1) '<proc-5>)
                          (begin
                            (set! value_reg (apply vector-ref args_reg))
                            (set! k_reg k2_reg)
                            (set! pc apply-cont))
                          (if (eq? (car temp_1) '<proc-6>)
                              (begin
                                (set! value_reg
                                  (vector-set!
                                    (car args_reg)
                                    (cadr args_reg)
                                    (caddr args_reg)))
                                (set! k_reg k2_reg)
                                (set! pc apply-cont))
                              (if (eq? (car temp_1) '<proc-7>)
                                  (begin
                                    (set! value_reg (make-vector args_reg))
                                    (set! k_reg k2_reg)
                                    (set! pc apply-cont))
                                  (if (eq? (car temp_1) '<proc-8>)
                                      (begin
                                        (apply printf-prim args_reg)
                                        (set! value_reg '<void>)
                                        (set! k_reg k2_reg)
                                        (set! pc apply-cont))
                                      (if (eq? (car temp_1) '<proc-9>)
                                          (begin
                                            (set! value_reg (not (car args_reg)))
                                            (set! k_reg k2_reg)
                                            (set! pc apply-cont))
                                          (if (eq? (car temp_1) '<proc-10>)
                                              (begin
                                                (set! value_reg (using-prim args_reg env2_reg))
                                                (set! k_reg k2_reg)
                                                (set! pc apply-cont))
                                              (if (eq? (car temp_1) '<proc-11>)
                                                  (begin
                                                    (set! value_reg env2_reg)
                                                    (set! k_reg k2_reg)
                                                    (set! pc apply-cont))
                                                  (if (eq? (car temp_1) '<proc-12>)
                                                      (begin
                                                        (set! k_reg k2_reg)
                                                        (set! env_reg env2_reg)
                                                        (set! lists_reg (cdr args_reg))
                                                        (set! proc_reg (car args_reg))
                                                        (set! pc for-each-prim))
                                                      (if (eq? (car temp_1) '<proc-13>)
                                                          (begin
                                                            (set! k_reg k2_reg)
                                                            (set! env_reg env2_reg)
                                                            (set! proc_reg (car args_reg))
                                                            (set! args_reg (cdr args_reg))
                                                            (set! pc map-prim))
                                                          (if (eq? (car temp_1) '<proc-14>)
                                                              (begin
                                                                (set! value_reg (get-current-time))
                                                                (set! k_reg k2_reg)
                                                                (set! pc apply-cont))
                                                              (if (eq? (car temp_1) '<proc-15>)
                                                                  (begin
                                                                    (set! value_reg (dir args_reg env2_reg))
                                                                    (set! k_reg k2_reg)
                                                                    (set! pc apply-cont))
                                                                  (if (eq? (car temp_1) '<proc-16>)
                                                                      (begin
                                                                        (set! value_reg (apply make-vector args_reg))
                                                                        (set! k_reg k2_reg)
                                                                        (set! pc apply-cont))
                                                                      (if (eq? (car temp_1) '<proc-17>)
                                                                          (begin
                                                                            (set! value_reg (apply append args_reg))
                                                                            (set! k_reg k2_reg)
                                                                            (set! pc apply-cont))
                                                                          (if (eq? (car temp_1) '<proc-18>)
                                                                              (begin
                                                                                (set! value_reg (apply reverse args_reg))
                                                                                (set! k_reg k2_reg)
                                                                                (set! pc apply-cont))
                                                                              (if (eq? (car temp_1) '<proc-19>)
                                                                                  (begin
                                                                                    (set! k_reg k2_reg)
                                                                                    (set! env_reg env2_reg)
                                                                                    (set! proc_reg (car args_reg))
                                                                                    (set! pc call/cc-primitive))
                                                                                  (if (eq? (car temp_1) '<proc-20>)
                                                                                      (begin
                                                                                        (set! k_reg k2_reg)
                                                                                        (set! env_reg env2_reg)
                                                                                        (set! pc get-primitive))
                                                                                      (if (eq? (car temp_1) '<proc-21>)
                                                                                          (begin
                                                                                            (set! k_reg k2_reg)
                                                                                            (set! env_reg env2_reg)
                                                                                            (set! pc import-primitive))
                                                                                          (if (eq? (car temp_1) '<proc-22>)
                                                                                              (begin
                                                                                                (set! value_reg (apply set-cdr! args_reg))
                                                                                                (set! k_reg k2_reg)
                                                                                                (set! pc apply-cont))
                                                                                              (if (eq? (car temp_1) '<proc-23>)
                                                                                                  (begin
                                                                                                    (set! value_reg (apply set-car! args_reg))
                                                                                                    (set! k_reg k2_reg)
                                                                                                    (set! pc apply-cont))
                                                                                                  (if (eq? (car temp_1) '<proc-24>)
                                                                                                      (begin
                                                                                                        (set! value_reg (apply range args_reg))
                                                                                                        (set! k_reg k2_reg)
                                                                                                        (set! pc apply-cont))
                                                                                                      (if (eq? (car temp_1) '<proc-25>)
                                                                                                          (begin
                                                                                                            (set! value_reg (apply memq args_reg))
                                                                                                            (set! k_reg k2_reg)
                                                                                                            (set! pc apply-cont))
                                                                                                          (if (eq? (car temp_1) '<proc-26>)
                                                                                                              (begin
                                                                                                                (set! value_reg (apply eq? args_reg))
                                                                                                                (set! k_reg k2_reg)
                                                                                                                (set! pc apply-cont))
                                                                                                              (if (eq? (car temp_1) '<proc-27>)
                                                                                                                  (begin
                                                                                                                    (set! value_reg (apply equal? args_reg))
                                                                                                                    (set! k_reg k2_reg)
                                                                                                                    (set! pc apply-cont))
                                                                                                                  (if (eq? (car temp_1) '<proc-28>)
                                                                                                                      (begin
                                                                                                                        (set! value_reg (apply = args_reg))
                                                                                                                        (set! k_reg k2_reg)
                                                                                                                        (set! pc apply-cont))
                                                                                                                      (if (eq? (car temp_1) '<proc-29>)
                                                                                                                          (begin
                                                                                                                            (set! value_reg (apply > args_reg))
                                                                                                                            (set! k_reg k2_reg)
                                                                                                                            (set! pc apply-cont))
                                                                                                                          (if (eq? (car temp_1) '<proc-30>)
                                                                                                                              (begin
                                                                                                                                (set! value_reg (apply < args_reg))
                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                (set! pc apply-cont))
                                                                                                                              (if (eq? (car temp_1) '<proc-31>)
                                                                                                                                  (if (= (length args_reg) 1)
                                                                                                                                      (if (= (car args_reg) 0)
                                                                                                                                          (begin
                                                                                                                                            (set! exception_reg "division by zero")
                                                                                                                                            (set! pc apply-handler))
                                                                                                                                          (begin
                                                                                                                                            (set! value_reg (apply / args_reg))
                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                            (set! pc apply-cont)))
                                                                                                                                      (if (>= (length args_reg) 2)
                                                                                                                                          (if (= (cadr args_reg) 0)
                                                                                                                                              (begin
                                                                                                                                                (set! exception_reg "division by zero")
                                                                                                                                                (set! pc apply-handler))
                                                                                                                                              (begin
                                                                                                                                                (set! value_reg (apply / args_reg))
                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                (set! pc apply-cont)))
                                                                                                                                          (begin
                                                                                                                                            (set! exception_reg "not enough args to /")
                                                                                                                                            (set! pc apply-handler))))
                                                                                                                                  (if (eq? (car temp_1) '<proc-32>)
                                                                                                                                      (begin
                                                                                                                                        (set! value_reg (apply * args_reg))
                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                        (set! pc apply-cont))
                                                                                                                                      (if (eq? (car temp_1) '<proc-33>)
                                                                                                                                          (begin
                                                                                                                                            (set! value_reg (apply - args_reg))
                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                            (set! pc apply-cont))
                                                                                                                                          (if (eq? (car temp_1) '<proc-34>)
                                                                                                                                              (begin
                                                                                                                                                (set! value_reg (apply + args_reg))
                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                (set! pc apply-cont))
                                                                                                                                              (if (eq? (car temp_1) '<proc-35>)
                                                                                                                                                  (begin
                                                                                                                                                    (set! value_reg args_reg)
                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                  (if (eq? (car temp_1) '<proc-36>)
                                                                                                                                                      (begin
                                                                                                                                                        (set! value_reg (apply cdr args_reg))
                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                      (if (eq? (car temp_1) '<proc-37>)
                                                                                                                                                          (begin
                                                                                                                                                            (set! value_reg (apply car args_reg))
                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                            (set! pc apply-cont))
                                                                                                                                                          (if (eq? (car temp_1) '<proc-38>)
                                                                                                                                                              (begin
                                                                                                                                                                (set! value_reg (apply cons args_reg))
                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                              (if (eq? (car temp_1) '<proc-39>)
                                                                                                                                                                  (begin
                                                                                                                                                                    (set! value_reg (apply null? args_reg))
                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                                  (if (eq? (car temp_1) '<proc-40>)
                                                                                                                                                                      (begin
                                                                                                                                                                        (set! load-stack '())
                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                        (set! env_reg toplevel-env)
                                                                                                                                                                        (set! filename_reg (car args_reg))
                                                                                                                                                                        (set! pc load-file))
                                                                                                                                                                      (if (eq? (car temp_1) '<proc-41>)
                                                                                                                                                                          (begin
                                                                                                                                                                            (newline-prim)
                                                                                                                                                                            (set! value_reg '<void>)
                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                            (set! pc apply-cont))
                                                                                                                                                                          (if (eq? (car temp_1) '<proc-42>)
                                                                                                                                                                              (begin
                                                                                                                                                                                (apply display-prim args_reg)
                                                                                                                                                                                (set! value_reg '<void>)
                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                              (if (eq? (car temp_1) '<proc-43>)
                                                                                                                                                                                  (begin
                                                                                                                                                                                    (for-each pretty-print-prim args_reg)
                                                                                                                                                                                    (set! value_reg '<void>)
                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-44>)
                                                                                                                                                                                      (begin
                                                                                                                                                                                        (set! value_reg (apply sqrt args_reg))
                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                        (set! pc apply-cont))
                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-45>)
                                                                                                                                                                                          (let ((proc 'undefined) (proc-args 'undefined))
                                                                                                                                                                                            (set! proc-args (cadr args_reg))
                                                                                                                                                                                            (set! proc (car args_reg))
                                                                                                                                                                                            (set! args_reg proc-args)
                                                                                                                                                                                            (set! proc_reg proc)
                                                                                                                                                                                            (set! pc apply-proc))
                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-46>)
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (set! k_reg (make-cont2 '<cont2-23> handler_reg k2_reg))
                                                                                                                                                                                                (set! input_reg (car args_reg))
                                                                                                                                                                                                (set! pc read-datum))
                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-47>)
                                                                                                                                                                                                  (begin
                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                    (set! datum_reg (car args_reg))
                                                                                                                                                                                                    (set! pc parse))
                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-48>)
                                                                                                                                                                                                      (begin
                                                                                                                                                                                                        (set! k_reg (make-cont '<cont-74> handler_reg k2_reg))
                                                                                                                                                                                                        (set! datum_reg (car args_reg))
                                                                                                                                                                                                        (set! pc parse))
                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-49>)
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! macro-env (make-macro-env))
                                                                                                                                                                                                            (set! toplevel-env (make-toplevel-env))
                                                                                                                                                                                                            (set! load-stack '())
                                                                                                                                                                                                            (set! final_reg (list 'exiting 'the 'interpreter))
                                                                                                                                                                                                            (set! pc #f))
                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-50>)
                                                                                                                                                                                                              (let ((k 'undefined))
                                                                                                                                                                                                                (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                (set! value_reg (car args_reg))
                                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-51>)
                                                                                                                                                                                                                  (let ((external-function-object 'undefined))
                                                                                                                                                                                                                    (set! external-function-object (list-ref temp_1 1))
                                                                                                                                                                                                                    (set! value_reg (apply* external-function-object args_reg))
                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                    (set! pc apply-cont))
                                                                                                                                                                                                                  (error 'apply-proc
                                                                                                                                                                                                                    "bad procedure: ~a"
                                                                                                                                                                                                                    proc_reg)))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define make-macro
  (lambda args (return* (cons 'macro-transformer args))))

(define*
  apply-macro
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr macro_reg))
      (if (eq? (car temp_1) '<macro-1>)
          (let ((name 'undefined)
                (formals 'undefined)
                (bodies 'undefined))
            (set! bodies (cddr datum_reg))
            (set! formals (cdadr datum_reg))
            (set! name (caadr datum_reg))
            (set! value_reg
              (list 'define name (cons 'lambda (cons formals bodies))))
            (set! pc apply-cont))
          (if (eq? (car temp_1) '<macro-2>)
              (let ((exps 'undefined))
                (set! exps (cdr datum_reg))
                (if (null? exps)
                    (begin (set! value_reg #t) (set! pc apply-cont))
                    (if (null? (cdr exps))
                        (begin (set! value_reg (car exps)) (set! pc apply-cont))
                        (begin
                          (set! value_reg
                            (list 'if (car exps) (cons 'and (cdr exps)) #f))
                          (set! pc apply-cont)))))
              (if (eq? (car temp_1) '<macro-3>)
                  (let ((exps 'undefined))
                    (set! exps (cdr datum_reg))
                    (if (null? exps)
                        (begin (set! value_reg #f) (set! pc apply-cont))
                        (if (null? (cdr exps))
                            (begin (set! value_reg (car exps)) (set! pc apply-cont))
                            (begin
                              (set! value_reg
                                (list
                                  'let
                                  (list
                                    (list 'bool (car exps))
                                    (list 'else-code (list 'lambda '() (cons 'or (cdr exps)))))
                                  (list 'if 'bool 'bool (list 'else-code))))
                              (set! pc apply-cont)))))
                  (if (eq? (car temp_1) '<macro-4>)
                      (let ((clauses 'undefined))
                        (set! clauses (cdr datum_reg))
                        (if (null? clauses)
                            (error 'cond-transformer
                              "bad concrete syntax: ~a"
                              datum_reg)
                            (let ((first-clause 'undefined) (other-clauses 'undefined))
                              (set! other-clauses (cdr clauses))
                              (set! first-clause (car clauses))
                              (if (or (null? first-clause) (not (list? first-clause)))
                                  (error 'cond-transformer
                                    "bad concrete syntax: ~a"
                                    datum_reg)
                                  (let ((test-exp 'undefined) (then-exps 'undefined))
                                    (set! then-exps (cdr first-clause))
                                    (set! test-exp (car first-clause))
                                    (if (eq? test-exp 'else)
                                        (if (null? then-exps)
                                            (error 'cond-transformer "bad concrete syntax: (~a)" 'else)
                                            (if (null? (cdr then-exps))
                                                (begin
                                                  (set! value_reg (car then-exps))
                                                  (set! pc apply-cont))
                                                (begin
                                                  (set! value_reg (cons 'begin then-exps))
                                                  (set! pc apply-cont))))
                                        (if (null? then-exps)
                                            (if (null? other-clauses)
                                                (begin
                                                  (set! value_reg
                                                    (list
                                                      'let
                                                      (list (list 'bool test-exp))
                                                      (list 'if 'bool 'bool)))
                                                  (set! pc apply-cont))
                                                (begin
                                                  (set! value_reg
                                                    (list
                                                      'let
                                                      (list
                                                        (list 'bool test-exp)
                                                        (list
                                                          'else-code
                                                          (list 'lambda '() (cons 'cond other-clauses))))
                                                      (list 'if 'bool 'bool (list 'else-code))))
                                                  (set! pc apply-cont)))
                                            (if (null? other-clauses)
                                                (if (null? (cdr then-exps))
                                                    (begin
                                                      (set! value_reg (list 'if test-exp (car then-exps)))
                                                      (set! pc apply-cont))
                                                    (begin
                                                      (set! value_reg (list 'if test-exp (cons 'begin then-exps)))
                                                      (set! pc apply-cont)))
                                                (if (null? (cdr then-exps))
                                                    (begin
                                                      (set! value_reg
                                                        (list
                                                          'if
                                                          test-exp
                                                          (car then-exps)
                                                          (cons 'cond other-clauses)))
                                                      (set! pc apply-cont))
                                                    (begin
                                                      (set! value_reg
                                                        (list
                                                          'if
                                                          test-exp
                                                          (cons 'begin then-exps)
                                                          (cons 'cond other-clauses)))
                                                      (set! pc apply-cont)))))))))))
                      (if (eq? (car temp_1) '<macro-5>)
                          (if (symbol? (cadr datum_reg))
                              (let ((name 'undefined)
                                    (bindings 'undefined)
                                    (vars 'undefined)
                                    (exps 'undefined)
                                    (bodies 'undefined))
                                (set! name (cadr datum_reg))
                                (set! bindings (caddr datum_reg))
                                (set! vars (map car bindings))
                                (set! exps (map cadr bindings))
                                (set! bodies (cdddr datum_reg))
                                (set! value_reg
                                  (list
                                    'letrec
                                    (list (list name (cons 'lambda (cons vars bodies))))
                                    (cons name exps)))
                                (set! pc apply-cont))
                              (let ((bindings 'undefined)
                                    (vars 'undefined)
                                    (exps 'undefined)
                                    (bodies 'undefined))
                                (set! bindings (cadr datum_reg))
                                (set! vars (map car bindings))
                                (set! exps (map cadr bindings))
                                (set! bodies (cddr datum_reg))
                                (set! value_reg
                                  (cons (cons 'lambda (cons vars bodies)) exps))
                                (set! pc apply-cont)))
                          (if (eq? (car temp_1) '<macro-6>)
                              (let ((decls 'undefined)
                                    (vars 'undefined)
                                    (procs 'undefined)
                                    (bodies 'undefined))
                                (set! decls (cadr datum_reg))
                                (set! vars (map car decls))
                                (set! procs (map cadr decls))
                                (set! bodies (cddr datum_reg))
                                (set! k2_reg (make-cont2 '<cont2-12> bodies k_reg))
                                (set! procs_reg procs)
                                (set! vars_reg vars)
                                (set! pc create-letrec-assignments))
                              (if (eq? (car temp_1) '<macro-7>)
                                  (let ((bindings 'undefined) (bodies 'undefined))
                                    (set! bodies (cddr datum_reg))
                                    (set! bindings (cadr datum_reg))
                                    (set! bodies_reg bodies)
                                    (set! bindings_reg bindings)
                                    (set! pc nest-let*-bindings))
                                  (if (eq? (car temp_1) '<macro-8>)
                                      (let ((exp 'undefined) (clauses 'undefined))
                                        (set! clauses (cddr datum_reg))
                                        (set! exp (cadr datum_reg))
                                        (if (symbol? exp)
                                            (begin
                                              (set! k_reg (make-cont '<cont-12> k_reg))
                                              (set! clauses_reg clauses)
                                              (set! var_reg exp)
                                              (set! pc case-clauses->simple-cond-clauses))
                                            (begin
                                              (set! k2_reg (make-cont2 '<cont2-14> exp k_reg))
                                              (set! clauses_reg clauses)
                                              (set! var_reg 'r)
                                              (set! pc case-clauses->cond-clauses))))
                                      (if (eq? (car temp_1) '<macro-9>)
                                          (let ((exp 'undefined) (clauses 'undefined))
                                            (set! clauses (cddr datum_reg))
                                            (set! exp (cadr datum_reg))
                                            (if (symbol? exp)
                                                (begin
                                                  (set! k2_reg (make-cont2 '<cont2-16> k_reg))
                                                  (set! clauses_reg clauses)
                                                  (set! var_reg exp)
                                                  (set! pc record-case-clauses->cond-clauses))
                                                (begin
                                                  (set! k2_reg (make-cont2 '<cont2-14> exp k_reg))
                                                  (set! clauses_reg clauses)
                                                  (set! var_reg 'r)
                                                  (set! pc record-case-clauses->cond-clauses))))
                                          (error 'apply-macro
                                            "bad macro-transformer: ~a"
                                            macro_reg)))))))))))))

(define 1st
  (lambda (n) (return* (string-ref chars-to-scan n))))

(define remaining (lambda (n) (return* (+ 1 n))))

(define*
  scan-input
  (lambda ()
    (set! chars-to-scan
      (string-append input_reg (string #\nul)))
    (set! chars_reg 0)
    (set! pc scan-input-loop)))

(define*
  scan-input-loop
  (lambda ()
    (set! k_reg (make-cont2 '<cont2-1> handler_reg k_reg))
    (set! buffer_reg '())
    (set! action_reg (list 'goto 'start-state))
    (set! pc apply-action)))

(define scan-string
  (lambda (input)
    (set! read-line-count 1)
    (set! read-char-count 0)
    (set! k_reg init-cont)
    (set! handler_reg init-handler)
    (set! input_reg input)
    (set! pc scan-input)))

(define scan-file
  (lambda (filename)
    (set! read-line-count 1)
    (set! read-char-count 0)
    (set! k_reg init-cont)
    (set! handler_reg init-handler)
    (set! input_reg (read-content filename))
    (set! pc scan-input)))

(define*
  apply-action
  (lambda ()
    (if (eq? (car action_reg) 'shift)
        (let ((next 'undefined))
          (set! next (list-ref action_reg 1))
          (set! read-char-count (+ read-char-count 1))
          (set! buffer_reg (cons (1st chars_reg) buffer_reg))
          (set! chars_reg (remaining chars_reg))
          (set! action_reg next)
          (set! pc apply-action))
        (if (eq? (car action_reg) 'replace)
            (let ((new-char 'undefined) (next 'undefined))
              (set! next (list-ref action_reg 2))
              (set! new-char (list-ref action_reg 1))
              (set! chars_reg (remaining chars_reg))
              (set! buffer_reg (cons new-char buffer_reg))
              (set! action_reg next)
              (set! pc apply-action))
            (if (eq? (car action_reg) 'drop-newline)
                (let ((next 'undefined))
                  (set! next (list-ref action_reg 1))
                  (set! read-line-count (+ read-line-count 1))
                  (set! read-char-count 0)
                  (set! chars_reg (remaining chars_reg))
                  (set! action_reg next)
                  (set! pc apply-action))
                (if (eq? (car action_reg) 'drop)
                    (let ((next 'undefined))
                      (set! next (list-ref action_reg 1))
                      (set! read-char-count (+ read-char-count 1))
                      (set! chars_reg (remaining chars_reg))
                      (set! action_reg next)
                      (set! pc apply-action))
                    (if (eq? (car action_reg) 'goto)
                        (let ((state 'undefined))
                          (set! state (list-ref action_reg 1))
                          (let ((action 'undefined))
                            (set! action (apply-state state (1st chars_reg)))
                            (if (eq? action 'error)
                                (set! pc scan-error)
                                (begin (set! action_reg action) (set! pc apply-action)))))
                        (if (eq? (car action_reg) 'emit)
                            (let ((token-type 'undefined))
                              (set! token-type (list-ref action_reg 1))
                              (set! k_reg (make-cont '<cont-3> chars_reg k_reg))
                              (set! token-type_reg token-type)
                              (set! pc convert-buffer-to-token))
                            (error 'apply-action
                              "invalid action: ~a"
                              action_reg)))))))))

(define*
  scan-error
  (lambda ()
    (let ((c 'undefined))
      (set! c (1st chars_reg))
      (if (char=? c #\nul)
          (begin
            (set! exception_reg
              (format
                "unexpected end of input at line: ~a col: ~a"
                read-line-count
                read-char-count))
            (set! pc apply-handler))
          (begin
            (set! exception_reg
              (format
                "unexpected character ~a encountered at line: ~a col: ~a"
                c
                read-line-count
                read-char-count))
            (set! pc apply-handler))))))

(define*
  convert-buffer-to-token
  (lambda ()
    (let ((buffer 'undefined))
      (set! buffer (reverse buffer_reg))
      (if (eq? token-type_reg 'integer)
          (begin
            (set! value_reg (list 'integer (list->string buffer)))
            (set! pc apply-cont))
          (if (eq? token-type_reg 'decimal)
              (begin
                (set! value_reg (list 'decimal (list->string buffer)))
                (set! pc apply-cont))
              (if (eq? token-type_reg 'rational)
                  (begin
                    (set! value_reg (list 'rational (list->string buffer)))
                    (set! pc apply-cont))
                  (if (eq? token-type_reg 'identifier)
                      (begin
                        (set! value_reg
                          (list 'identifier (string->symbol (list->string buffer))))
                        (set! pc apply-cont))
                      (if (eq? token-type_reg 'boolean)
                          (begin
                            (set! value_reg
                              (list
                                'boolean
                                (or (char=? (car buffer) #\t) (char=? (car buffer) #\T))))
                            (set! pc apply-cont))
                          (if (eq? token-type_reg 'character)
                              (begin
                                (set! value_reg (list 'character (car buffer)))
                                (set! pc apply-cont))
                              (if (eq? token-type_reg 'named-character)
                                  (let ((name 'undefined))
                                    (set! name (list->string buffer))
                                    (if (string=? name "nul")
                                        (begin
                                          (set! value_reg (list 'character #\nul))
                                          (set! pc apply-cont))
                                        (if (string=? name "space")
                                            (begin
                                              (set! value_reg (list 'character #\space))
                                              (set! pc apply-cont))
                                            (if (string=? name "tab")
                                                (begin
                                                  (set! value_reg (list 'character #\tab))
                                                  (set! pc apply-cont))
                                                (if (string=? name "newline")
                                                    (begin
                                                      (set! value_reg (list 'character #\newline))
                                                      (set! pc apply-cont))
                                                    (if (string=? name "linefeed")
                                                        (begin
                                                          (set! value_reg (list 'character #\newline))
                                                          (set! pc apply-cont))
                                                        (if (string=? name "backspace")
                                                            (begin
                                                              (set! value_reg (list 'character #\backspace))
                                                              (set! pc apply-cont))
                                                            (if (string=? name "return")
                                                                (begin
                                                                  (set! value_reg (list 'character #\return))
                                                                  (set! pc apply-cont))
                                                                (if (string=? name "page")
                                                                    (begin
                                                                      (set! value_reg (list 'character #\page))
                                                                      (set! pc apply-cont))
                                                                    (begin
                                                                      (set! exception_reg
                                                                        (format
                                                                          "invalid character name '~a' at line: ~a col: ~a"
                                                                          name
                                                                          read-line-count
                                                                          read-char-count))
                                                                      (set! pc apply-handler)))))))))))
                                  (if (eq? token-type_reg 'string)
                                      (begin
                                        (set! value_reg (list 'string (list->string buffer)))
                                        (set! pc apply-cont))
                                      (begin
                                        (set! value_reg (list token-type_reg))
                                        (set! pc apply-cont)))))))))))))

(define token-type?
  (lambda (token class) (return* (eq? (car token) class))))

(define char-delimiter?
  (lambda (c)
    (return*
      (or (char-whitespace? c)
          (char=? c #\')
          (char=? c #\()
          (char=? c #\[)
          (char=? c #\))
          (char=? c #\])
          (char=? c #\")
          (char=? c #\;)
          (char=? c #\#)
          (char=? c #\nul)))))

(define char-initial?
  (lambda (c)
    (return*
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
          (char=? c #\~)))))

(define char-special-subsequent?
  (lambda (c)
    (return*
      (or (char=? c #\+)
          (char=? c #\-)
          (char=? c #\@)
          (char=? c #\.)))))

(define char-subsequent?
  (lambda (c)
    (return*
      (or (char-initial? c)
          (char-numeric? c)
          (char-special-subsequent? c)))))

(define char-sign?
  (lambda (c) (return* (or (char=? c #\+) (char=? c #\-)))))

(define char-boolean?
  (lambda (c)
    (return*
      (or (char=? c #\t)
          (char=? c #\T)
          (char=? c #\f)
          (char=? c #\F)))))

(define apply-state
  (lambda (state c)
    (if (eq? state 'start-state)
        (if (char=? c #\newline)
            (return* (list 'drop-newline (list 'goto 'start-state)))
            (if (char-whitespace? c)
                (return* (list 'drop (list 'goto 'start-state)))
                (if (char=? c #\;)
                    (return* (list 'drop (list 'goto 'comment-state)))
                    (if (char=? c #\()
                        (return* (list 'drop (list 'emit 'lparen)))
                        (if (char=? c #\[)
                            (return* (list 'drop (list 'emit 'lbracket)))
                            (if (char=? c #\))
                                (return* (list 'drop (list 'emit 'rparen)))
                                (if (char=? c #\])
                                    (return* (list 'drop (list 'emit 'rbracket)))
                                    (if (char=? c #\')
                                        (return* (list 'drop (list 'emit 'apostrophe)))
                                        (if (char=? c #\`)
                                            (return* (list 'drop (list 'emit 'backquote)))
                                            (if (char=? c #\,)
                                                (return* (list 'drop (list 'goto 'comma-state)))
                                                (if (char=? c #\#)
                                                    (return* (list 'drop (list 'goto 'hash-prefix-state)))
                                                    (if (char=? c #\")
                                                        (return* (list 'drop (list 'goto 'string-state)))
                                                        (if (char-initial? c)
                                                            (return* (list 'shift (list 'goto 'identifier-state)))
                                                            (if (char-sign? c)
                                                                (return* (list 'shift (list 'goto 'signed-state)))
                                                                (if (char=? c #\.)
                                                                    (return* (list 'shift (list 'goto 'decimal-point-state)))
                                                                    (if (char-numeric? c)
                                                                        (return* (list 'shift (list 'goto 'whole-number-state)))
                                                                        (if (char=? c #\nul)
                                                                            (return* (list 'drop (list 'emit 'end-marker)))
                                                                            (return* 'error))))))))))))))))))
        (if (eq? state 'comment-state)
            (if (char=? c #\newline)
                (return* (list 'drop-newline (list 'goto 'start-state)))
                (if (char=? c #\nul)
                    (return* (list 'goto 'start-state))
                    (return* (list 'drop (list 'goto 'comment-state)))))
            (if (eq? state 'comma-state)
                (if (char=? c #\@)
                    (return* (list 'drop (list 'emit 'comma-at)))
                    (return* (list 'emit 'comma)))
                (if (eq? state 'hash-prefix-state)
                    (if (char-boolean? c)
                        (return* (list 'shift (list 'emit 'boolean)))
                        (if (char=? c #\\)
                            (return* (list 'drop (list 'goto 'character-state)))
                            (if (char=? c #\()
                                (return* (list 'drop (list 'emit 'lvector)))
                                (return* 'error))))
                    (if (eq? state 'character-state)
                        (if (char-alphabetic? c)
                            (return*
                              (list 'shift (list 'goto 'alphabetic-character-state)))
                            (if (not (char=? c #\nul))
                                (return* (list 'shift (list 'emit 'character)))
                                (return* 'error)))
                        (if (eq? state 'alphabetic-character-state)
                            (if (char-alphabetic? c)
                                (return* (list 'shift (list 'goto 'named-character-state)))
                                (return* (list 'emit 'character)))
                            (if (eq? state 'named-character-state)
                                (if (char-delimiter? c)
                                    (return* (list 'emit 'named-character))
                                    (return* (list 'shift (list 'goto 'named-character-state))))
                                (if (eq? state 'string-state)
                                    (if (char=? c #\")
                                        (return* (list 'drop (list 'emit 'string)))
                                        (if (char=? c #\\)
                                            (return* (list 'drop (list 'goto 'string-escape-state)))
                                            (if (char=? c #\nul)
                                                (return* 'error)
                                                (return* (list 'shift (list 'goto 'string-state))))))
                                    (if (eq? state 'string-escape-state)
                                        (if (char=? c #\")
                                            (return* (list 'shift (list 'goto 'string-state)))
                                            (if (char=? c #\\)
                                                (return* (list 'shift (list 'goto 'string-state)))
                                                (if (char=? c #\b)
                                                    (return* (list 'replace "\b" (list 'goto 'string-state)))
                                                    (if (char=? c #\f)
                                                        (return* (list 'replace "\f" (list 'goto 'string-state)))
                                                        (if (char=? c #\n)
                                                            (return* (list 'replace "\n" (list 'goto 'string-state)))
                                                            (if (char=? c #\t)
                                                                (return* (list 'replace "\t" (list 'goto 'string-state)))
                                                                (if (char=? c #\r)
                                                                    (return* (list 'replace "\r" (list 'goto 'string-state)))
                                                                    (return* 'error))))))))
                                        (if (eq? state 'identifier-state)
                                            (if (char-subsequent? c)
                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                (if (char-delimiter? c)
                                                    (return* (list 'emit 'identifier))
                                                    (return* 'error)))
                                            (if (eq? state 'signed-state)
                                                (if (char-numeric? c)
                                                    (return* (list 'shift (list 'goto 'whole-number-state)))
                                                    (if (char=? c #\.)
                                                        (return*
                                                          (list 'shift (list 'goto 'signed-decimal-point-state)))
                                                        (if (char-delimiter? c)
                                                            (return* (list 'emit 'identifier))
                                                            (if (char-subsequent? c)
                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                (return* 'error)))))
                                                (if (eq? state 'decimal-point-state)
                                                    (if (char-numeric? c)
                                                        (return*
                                                          (list 'shift (list 'goto 'fractional-number-state)))
                                                        (if (char-delimiter? c)
                                                            (return* (list 'emit 'dot))
                                                            (if (char-subsequent? c)
                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                (return* 'error))))
                                                    (if (eq? state 'signed-decimal-point-state)
                                                        (if (char-numeric? c)
                                                            (return*
                                                              (list 'shift (list 'goto 'fractional-number-state)))
                                                            (if (char-delimiter? c)
                                                                (return* (list 'emit 'identifier))
                                                                (if (char-subsequent? c)
                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                    (return* 'error))))
                                                        (if (eq? state 'whole-number-state)
                                                            (if (char-numeric? c)
                                                                (return* (list 'shift (list 'goto 'whole-number-state)))
                                                                (if (char=? c #\.)
                                                                    (return*
                                                                      (list 'shift (list 'goto 'fractional-number-state)))
                                                                    (if (char=? c #\/)
                                                                        (return* (list 'shift (list 'goto 'rational-number-state)))
                                                                        (if (or (char=? c #\e) (char=? c #\E))
                                                                            (return* (list 'shift (list 'goto 'suffix-state)))
                                                                            (if (char-delimiter? c)
                                                                                (return* (list 'emit 'integer))
                                                                                (if (char-subsequent? c)
                                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                    (return* 'error)))))))
                                                            (if (eq? state 'fractional-number-state)
                                                                (if (char-numeric? c)
                                                                    (return*
                                                                      (list 'shift (list 'goto 'fractional-number-state)))
                                                                    (if (or (char=? c #\e) (char=? c #\E))
                                                                        (return* (list 'shift (list 'goto 'suffix-state)))
                                                                        (if (char-delimiter? c)
                                                                            (return* (list 'emit 'decimal))
                                                                            (if (char-subsequent? c)
                                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                (return* 'error)))))
                                                                (if (eq? state 'rational-number-state)
                                                                    (if (char-numeric? c)
                                                                        (return* (list 'shift (list 'goto 'rational-number-state*)))
                                                                        (if (char-delimiter? c)
                                                                            (return* (list 'emit 'identifier))
                                                                            (if (char-subsequent? c)
                                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                (return* 'error))))
                                                                    (if (eq? state 'rational-number-state*)
                                                                        (if (char-numeric? c)
                                                                            (return* (list 'shift (list 'goto 'rational-number-state*)))
                                                                            (if (char-delimiter? c)
                                                                                (return* (list 'emit 'rational))
                                                                                (if (char-subsequent? c)
                                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                    (return* 'error))))
                                                                        (if (eq? state 'suffix-state)
                                                                            (if (char-sign? c)
                                                                                (return* (list 'shift (list 'goto 'signed-exponent-state)))
                                                                                (if (char-numeric? c)
                                                                                    (return* (list 'shift (list 'goto 'exponent-state)))
                                                                                    (if (char-delimiter? c)
                                                                                        (return* (list 'emit 'identifier))
                                                                                        (if (char-subsequent? c)
                                                                                            (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                            (return* 'error)))))
                                                                            (if (eq? state 'signed-exponent-state)
                                                                                (if (char-numeric? c)
                                                                                    (return* (list 'shift (list 'goto 'exponent-state)))
                                                                                    (if (char-delimiter? c)
                                                                                        (return* (list 'emit 'identifier))
                                                                                        (if (char-subsequent? c)
                                                                                            (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                            (return* 'error))))
                                                                                (if (eq? state 'exponent-state)
                                                                                    (if (char-numeric? c)
                                                                                        (return* (list 'shift (list 'goto 'exponent-state)))
                                                                                        (if (char-delimiter? c)
                                                                                            (return* (list 'emit 'decimal))
                                                                                            (if (char-subsequent? c)
                                                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                                (return* 'error))))
                                                                                    (error 'apply-state
                                                                                      "invalid state: ~a"
                                                                                      state)))))))))))))))))))))))

(define first (lambda (x) (return* (car x))))

(define rest-of (lambda (x) (return* (cdr x))))

(define read-string
  (lambda (input)
    (set! k_reg init-cont2)
    (set! handler_reg init-handler)
    (set! input_reg input)
    (set! pc read-datum)))

(define*
  read-datum
  (lambda ()
    (set! read-char-count 0)
    (set! read-line-count 1)
    (set! k_reg (make-cont '<cont-4> handler_reg k_reg))
    (set! pc scan-input)))

(define get-line-count
  (lambda (token) (return* (rac (rdc token)))))

(define get-char-count
  (lambda (token) (return* (rac token))))

(define rac
  (lambda (lyst)
    (if (null? (cdr lyst))
        (return* (car lyst))
        (return* (rac (cdr lyst))))))

(define rdc
  (lambda (lyst)
    (if (null? (cdr lyst))
        (return* '())
        (return* (cons (car lyst) (rdc (cdr lyst)))))))

(define string->integer
  (lambda (str) (return* (string->number str))))

(define string->decimal
  (lambda (str) (return* (string->number str))))

(define string->rational
  (lambda (str) (return* (string->number str))))

(define true? (lambda (v) (if v (return* #t) (return* #f))))

(define*
  read-sexp
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (first tokens_reg))
      (if (eq? (car temp_1) 'integer)
          (let ((str 'undefined))
            (set! str (list-ref temp_1 1))
            (set! value2_reg (rest-of tokens_reg))
            (set! value1_reg (string->integer str))
            (set! pc apply-cont2))
          (if (eq? (car temp_1) 'decimal)
              (let ((str 'undefined))
                (set! str (list-ref temp_1 1))
                (set! value2_reg (rest-of tokens_reg))
                (set! value1_reg (string->decimal str))
                (set! pc apply-cont2))
              (if (eq? (car temp_1) 'rational)
                  (let ((str 'undefined))
                    (set! str (list-ref temp_1 1))
                    (let ((num 'undefined))
                      (set! num (string->rational str))
                      (if (true? num)
                          (begin
                            (set! value2_reg (rest-of tokens_reg))
                            (set! value1_reg num)
                            (set! pc apply-cont2))
                          (begin
                            (set! exception_reg
                              (format
                                "cannot represent ~a at line: ~a col: ~a"
                                str
                                (get-line-count (first tokens_reg))
                                (get-char-count (first tokens_reg))))
                            (set! pc apply-handler)))))
                  (if (eq? (car temp_1) 'boolean)
                      (let ((bool 'undefined))
                        (set! bool (list-ref temp_1 1))
                        (set! value2_reg (rest-of tokens_reg))
                        (set! value1_reg bool)
                        (set! pc apply-cont2))
                      (if (eq? (car temp_1) 'character)
                          (let ((char 'undefined))
                            (set! char (list-ref temp_1 1))
                            (set! value2_reg (rest-of tokens_reg))
                            (set! value1_reg char)
                            (set! pc apply-cont2))
                          (if (eq? (car temp_1) 'string)
                              (let ((str 'undefined))
                                (set! str (list-ref temp_1 1))
                                (set! value2_reg (rest-of tokens_reg))
                                (set! value1_reg str)
                                (set! pc apply-cont2))
                              (if (eq? (car temp_1) 'identifier)
                                  (let ((id 'undefined))
                                    (set! id (list-ref temp_1 1))
                                    (set! value2_reg (rest-of tokens_reg))
                                    (set! value1_reg id)
                                    (set! pc apply-cont2))
                                  (if (eq? (car temp_1) 'apostrophe)
                                      (begin
                                        (set! keyword_reg 'quote)
                                        (set! pc read-abbreviation))
                                      (if (eq? (car temp_1) 'backquote)
                                          (begin
                                            (set! keyword_reg 'quasiquote)
                                            (set! pc read-abbreviation))
                                          (if (eq? (car temp_1) 'comma)
                                              (begin
                                                (set! keyword_reg 'unquote)
                                                (set! pc read-abbreviation))
                                              (if (eq? (car temp_1) 'comma-at)
                                                  (begin
                                                    (set! keyword_reg 'unquote-splicing)
                                                    (set! pc read-abbreviation))
                                                  (if (eq? (car temp_1) 'lparen)
                                                      (let ((tokens 'undefined))
                                                        (set! tokens (rest-of tokens_reg))
                                                        (if (token-type? (first tokens) 'dot)
                                                            (begin (set! tokens_reg tokens) (set! pc read-error))
                                                            (begin
                                                              (set! expected-terminator_reg 'rparen)
                                                              (set! tokens_reg tokens)
                                                              (set! pc read-sexp-sequence))))
                                                      (if (eq? (car temp_1) 'lbracket)
                                                          (let ((tokens 'undefined))
                                                            (set! tokens (rest-of tokens_reg))
                                                            (if (token-type? (first tokens) 'dot)
                                                                (begin (set! tokens_reg tokens) (set! pc read-error))
                                                                (begin
                                                                  (set! expected-terminator_reg 'rbracket)
                                                                  (set! tokens_reg tokens)
                                                                  (set! pc read-sexp-sequence))))
                                                          (if (eq? (car temp_1) 'lvector)
                                                              (begin
                                                                (set! k_reg (make-cont2 '<cont2-4> k_reg))
                                                                (set! tokens_reg (rest-of tokens_reg))
                                                                (set! pc read-vector))
                                                              (set! pc read-error))))))))))))))))))

(define*
  read-abbreviation
  (lambda ()
    (set! k_reg (make-cont2 '<cont2-5> keyword_reg k_reg))
    (set! tokens_reg (rest-of tokens_reg))
    (set! pc read-sexp)))

(define*
  read-sexp-sequence
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (first tokens_reg))
      (if (memq (car temp_1) (list 'rparen 'rbracket))
          (begin (set! sexp_reg '()) (set! pc close-sexp-sequence))
          (if (eq? (car temp_1) 'dot)
              (begin
                (set! k_reg
                  (make-cont2
                    '<cont2-8>
                    expected-terminator_reg
                    handler_reg
                    k_reg))
                (set! tokens_reg (rest-of tokens_reg))
                (set! pc read-sexp))
              (begin
                (set! k_reg
                  (make-cont2
                    '<cont2-7>
                    expected-terminator_reg
                    handler_reg
                    k_reg))
                (set! pc read-sexp)))))))

(define*
  close-sexp-sequence
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (first tokens_reg))
      (if (memq (car temp_1) (list 'rparen 'rbracket))
          (if (token-type? (first tokens_reg) expected-terminator_reg)
              (begin
                (set! value2_reg (rest-of tokens_reg))
                (set! value1_reg sexp_reg)
                (set! pc apply-cont2))
              (if (eq? expected-terminator_reg 'rparen)
                  (begin
                    (set! exception_reg
                      (format
                        "parenthesized list terminated by bracket at line: ~a col: ~a"
                        (get-line-count (first tokens_reg))
                        (get-char-count (first tokens_reg))))
                    (set! pc apply-handler))
                  (if (eq? expected-terminator_reg 'rbracket)
                      (begin
                        (set! exception_reg
                          (format
                            "bracketed list terminated by parenthesis at line: ~a col: ~a"
                            (get-line-count (first tokens_reg))
                            (get-char-count (first tokens_reg))))
                        (set! pc apply-handler)))))
          (set! pc read-error)))))

(define*
  read-vector
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (first tokens_reg))
      (if (eq? (car temp_1) 'rparen)
          (begin
            (set! value2_reg (rest-of tokens_reg))
            (set! value1_reg '())
            (set! pc apply-cont2))
          (begin
            (set! k_reg (make-cont2 '<cont2-9> handler_reg k_reg))
            (set! pc read-sexp))))))

(define*
  read-error
  (lambda ()
    (let ((token 'undefined))
      (set! token (first tokens_reg))
      (if (token-type? token 'end-marker)
          (begin
            (set! exception_reg
              (format
                "unexpected end of input at line: ~a col: ~a"
                (get-line-count token)
                (get-char-count token)))
            (set! pc apply-handler))
          (begin
            (set! exception_reg
              (format
                "unexpected token ~a encountered at line: ~a col: ~a"
                (car token)
                (get-line-count token)
                (get-char-count token)))
            (set! pc apply-handler))))))

(define read-file
  (lambda (filename)
    (set! k_reg (make-cont '<cont-5>))
    (set! handler_reg init-handler)
    (set! input_reg (read-content filename))
    (set! pc scan-input)))

(define*
  print-unparsed-sexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
        (begin (set! value_reg 'done) (set! pc apply-cont))
        (begin
          (set! k_reg (make-cont2 '<cont2-10> handler_reg k_reg))
          (set! pc read-sexp)))))

(define read-next-sexp
  (lambda (tokens)
    (set! k_reg (make-cont2 '<cont2-11>))
    (set! handler_reg init-handler)
    (set! tokens_reg tokens)
    (set! pc read-sexp)))

(define read-content
  (lambda (filename)
    (return*
      (apply
        string
        (call-with-input-file
          filename
          (lambda (port)
            (let ((loop 'undefined))
              (set! loop
                (lambda (char)
                  (if (eof-object? char)
                      '()
                      (cons char (loop (read-char port))))))
              (loop (read-char port)))))))))

(define make-binding
  (lambda (variable value)
    (return* (list variable "" value))))

(define binding-variable
  (lambda (binding) (return* (car binding))))

(define binding-docstring
  (lambda (binding) (return* (cadr binding))))

(define binding-value
  (lambda (binding) (return* (caddr binding))))

(define set-binding-docstring!
  (lambda (binding docstring)
    (return* (set-car! (cdr binding) docstring))))

(define set-binding-value!
  (lambda (binding value)
    (return* (set-car! (cddr binding) value))))

(define make-frame
  (lambda (variables values)
    (return* (map make-binding variables values))))

(define first-binding
  (lambda (frame) (return* (car frame))))

(define rest-of-bindings
  (lambda (frame) (return* (cdr frame))))

(define empty-frame?
  (lambda (frame) (return* (null? frame))))

(define search-frame
  (lambda (frame variable)
    (if (empty-frame? frame)
        (return* #f)
        (if (eq? (binding-variable (first-binding frame)) variable)
            (return* (first-binding frame))
            (return*
              (search-frame (rest-of-bindings frame) variable))))))

(define environment?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) 'environment)))))

(define make-empty-environment
  (lambda () (return* (cons 'environment (list '())))))

(define make-initial-environment
  (lambda (vars vals)
    (return*
      (cons 'environment (list (make-frame vars vals))))))

(define first-frame (lambda (env) (return* (cadr env))))

(define frames (lambda (env) (return* (cdr env))))

(define set-first-frame!
  (lambda (env new-frame)
    (return* (set-car! (cdr env) new-frame))))

(define extend
  (lambda (env variables values)
    (return*
      (cons
        'environment
        (cons (make-frame variables values) (cdr env))))))

(define search-env
  (lambda (env variable)
    (return* (search-frames (cdr env) variable))))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
        (return* #f)
        (let ((binding 'undefined))
          (set! binding (search-frame (car frames) variable))
          (if binding
              (return* binding)
              (return* (search-frames (cdr frames) variable)))))))

(define*
  lookup-value
  (lambda ()
    (set! k_reg (make-cont '<cont-6> k_reg))
    (set! pc lookup-binding)))

(define*
  lookup-binding
  (lambda ()
    (let ((binding 'undefined))
      (set! binding (search-env env_reg variable_reg))
      (if binding
          (begin (set! value_reg binding) (set! pc apply-cont))
          (begin
            (set! k_reg
              (make-cont '<cont-7> variable_reg env_reg handler_reg
                k_reg))
            (set! pc split-variable))))))

(define dlr-env-contains (lambda (variable) (return* #t)))

(define dlr-env-lookup
  (lambda (variable) (return* (binding 42))))

(define*
  lookup-binding-in-first-frame
  (lambda ()
    (let ((frame 'undefined))
      (set! frame (first-frame env_reg))
      (let ((binding 'undefined))
        (set! binding (search-frame frame var_reg))
        (if binding
            (begin (set! value_reg binding) (set! pc apply-cont))
            (let ((new-binding 'undefined))
              (set! new-binding (make-binding var_reg 'undefined))
              (let ((new-frame 'undefined))
                (set! new-frame (cons new-binding frame))
                (set-first-frame! env_reg new-frame)
                (set! value_reg new-binding)
                (set! pc apply-cont))))))))

(define*
  lookup-variable-components
  (lambda ()
    (let ((var 'undefined))
      (set! var (car components_reg))
      (set! k_reg
        (make-cont '<cont-8> components_reg path_reg var handler_reg
          k_reg))
      (set! var_reg var)
      (set! pc lookup-module-binding))))

(define*
  lookup-module-binding
  (lambda ()
    (let ((binding 'undefined))
      (set! binding (search-env env_reg var_reg))
      (if binding
          (begin (set! value_reg binding) (set! pc apply-cont))
          (if (string=? path_reg "")
              (begin
                (set! exception_reg (format "unbound variable ~a" var_reg))
                (set! pc apply-handler))
              (begin
                (set! exception_reg
                  (format
                    "unbound variable ~a in module ~a"
                    var_reg
                    path_reg))
                (set! pc apply-handler)))))))

(define*
  split-variable
  (lambda ()
    (let ((strings 'undefined))
      (set! strings
        (group (string->list (symbol->string variable_reg)) #\.))
      (if (or (member "" strings) (= (length strings) 1))
          (begin (set! value_reg #f) (set! pc apply-cont))
          (begin
            (set! value_reg (map string->symbol strings))
            (set! pc apply-cont))))))

(define group
  (lambda (chars delimiter)
    (let ((position 'undefined) (group 'undefined))
      (set! position
        (lambda (chars)
          (if (char=? (car chars) delimiter)
              0
              (+ 1 (position (cdr chars))))))
      (set! group
        (lambda (chars)
          (if (null? chars)
              '()
              (if (not (member delimiter chars))
                  (list (apply string chars))
                  (let ((n 'undefined))
                    (set! n (position chars))
                    (cons
                      (apply string (list-head chars n))
                      (group (cdr (list-tail chars n)))))))))
      (return* (group chars)))))

(define syntactic-sugar?
  (lambda (datum)
    (return*
      (and (pair? datum)
           (symbol? (car datum))
           (true? (search-env macro-env (car datum)))))))

(define make-pattern-macro
  (lambda (clauses) (return* (cons 'pattern-macro clauses))))

(define macro-clauses
  (lambda (macro) (return* (cdr macro))))

(define pattern-macro?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) 'pattern-macro)))))

(define*
  expand-once
  (lambda ()
    (set! k_reg
      (make-cont '<cont-9> datum_reg handler_reg k_reg))
    (set! env_reg macro-env)
    (set! variable_reg (car datum_reg))
    (set! pc lookup-value)))

(define*
  process-macro-clauses
  (lambda ()
    (if (null? clauses_reg)
        (begin
          (set! exception_reg
            (format "no matching clause found for ~a" datum_reg))
          (set! pc apply-handler))
        (let ((left-pattern 'undefined) (right-pattern 'undefined))
          (set! right-pattern (cadar clauses_reg))
          (set! left-pattern (caar clauses_reg))
          (set! k_reg
            (make-cont '<cont-10> clauses_reg datum_reg right-pattern
              handler_reg k_reg))
          (set! p2_reg datum_reg)
          (set! p1_reg left-pattern)
          (set! pc unify-patterns)))))

(define*
  create-letrec-assignments
  (lambda ()
    (if (null? vars_reg)
        (begin
          (set! value2_reg '())
          (set! value1_reg '())
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (begin
          (set! k2_reg
            (make-cont2 '<cont2-13> procs_reg vars_reg k2_reg))
          (set! procs_reg (cdr procs_reg))
          (set! vars_reg (cdr vars_reg))
          (set! pc create-letrec-assignments)))))

(define*
  nest-let*-bindings
  (lambda ()
    (if (or (null? bindings_reg) (null? (cdr bindings_reg)))
        (begin
          (set! value_reg (cons 'let (cons bindings_reg bodies_reg)))
          (set! pc apply-cont))
        (begin
          (set! k_reg (make-cont '<cont-11> bindings_reg k_reg))
          (set! bindings_reg (cdr bindings_reg))
          (set! pc nest-let*-bindings)))))

(define*
  case-clauses->simple-cond-clauses
  (lambda ()
    (if (null? clauses_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-13> clauses_reg var_reg k_reg))
          (set! clauses_reg (cdr clauses_reg))
          (set! pc case-clauses->simple-cond-clauses)))))

(define*
  case-clauses->cond-clauses
  (lambda ()
    (if (null? clauses_reg)
        (begin
          (set! value2_reg '())
          (set! value1_reg '())
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (begin
          (set! k2_reg
            (make-cont2 '<cont2-15> clauses_reg var_reg k2_reg))
          (set! clauses_reg (cdr clauses_reg))
          (set! pc case-clauses->cond-clauses)))))

(define*
  record-case-clauses->cond-clauses
  (lambda ()
    (if (null? clauses_reg)
        (begin
          (set! value2_reg '())
          (set! value1_reg '())
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (begin
          (set! k2_reg
            (make-cont2 '<cont2-17> clauses_reg var_reg k2_reg))
          (set! clauses_reg (cdr clauses_reg))
          (set! pc record-case-clauses->cond-clauses)))))

(define make-macro-env
  (lambda ()
    (return*
      (make-initial-environment
        (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case)
        (list and-transformer or-transformer cond-transformer
          let-transformer letrec-transformer let*-transformer
          case-transformer record-case-transformer)))))

(define parse-string
  (lambda (string)
    (set! k_reg (make-cont2 '<cont2-18>))
    (set! handler_reg init-handler)
    (set! input_reg string)
    (set! pc read-datum)))

(define*
  parse
  (lambda ()
    (if (literal? datum_reg)
        (begin
          (set! value_reg (lit-exp datum_reg))
          (set! pc apply-cont))
        (if (quote? datum_reg)
            (begin
              (set! value_reg (lit-exp (cadr datum_reg)))
              (set! pc apply-cont))
            (if (quasiquote? datum_reg)
                (begin
                  (set! k_reg (make-cont '<cont-30> handler_reg k_reg))
                  (set! datum_reg (cadr datum_reg))
                  (set! pc expand-quasiquote))
                (if (unquote? datum_reg)
                    (begin
                      (set! exception_reg (format "misplaced ~a" datum_reg))
                      (set! pc apply-handler))
                    (if (unquote-splicing? datum_reg)
                        (begin
                          (set! exception_reg (format "misplaced ~a" datum_reg))
                          (set! pc apply-handler))
                        (if (symbol? datum_reg)
                            (begin
                              (set! value_reg (var-exp datum_reg))
                              (set! pc apply-cont))
                            (if (syntactic-sugar? datum_reg)
                                (begin
                                  (set! k_reg (make-cont '<cont-30> handler_reg k_reg))
                                  (set! pc expand-once))
                                (if (if-then? datum_reg)
                                    (begin
                                      (set! k_reg
                                        (make-cont '<cont-36> datum_reg handler_reg k_reg))
                                      (set! datum_reg (cadr datum_reg))
                                      (set! pc parse))
                                    (if (if-else? datum_reg)
                                        (begin
                                          (set! k_reg
                                            (make-cont '<cont-34> datum_reg handler_reg k_reg))
                                          (set! datum_reg (cadr datum_reg))
                                          (set! pc parse))
                                        (if (assignment? datum_reg)
                                            (begin
                                              (set! k_reg (make-cont '<cont-31> datum_reg k_reg))
                                              (set! datum_reg (caddr datum_reg))
                                              (set! pc parse))
                                            (if (define? datum_reg)
                                                (if (mit-style? datum_reg)
                                                    (begin
                                                      (set! k_reg (make-cont '<cont-30> handler_reg k_reg))
                                                      (set! macro_reg mit-define-transformer)
                                                      (set! pc apply-macro))
                                                    (if (= (length datum_reg) 3)
                                                        (begin
                                                          (set! k_reg (make-cont '<cont-29> datum_reg k_reg))
                                                          (set! datum_reg (caddr datum_reg))
                                                          (set! pc parse))
                                                        (begin
                                                          (set! k_reg
                                                            (make-cont '<cont-28> datum_reg handler_reg k_reg))
                                                          (set! datum_reg (cadddr datum_reg))
                                                          (set! pc parse))))
                                                (if (define-syntax? datum_reg)
                                                    (begin
                                                      (set! value_reg
                                                        (define-syntax-exp (cadr datum_reg) (cddr datum_reg)))
                                                      (set! pc apply-cont))
                                                    (if (begin? datum_reg)
                                                        (begin
                                                          (set! k_reg
                                                            (make-cont '<cont-26> datum_reg handler_reg k_reg))
                                                          (set! datum-list_reg (cdr datum_reg))
                                                          (set! pc parse-all))
                                                        (if (lambda? datum_reg)
                                                            (begin
                                                              (set! k_reg (make-cont '<cont-25> datum_reg k_reg))
                                                              (set! datum_reg (cons 'begin (cddr datum_reg)))
                                                              (set! pc parse))
                                                            (if (try? datum_reg)
                                                                (if (= (length datum_reg) 2)
                                                                    (begin
                                                                      (set! datum_reg (try-body datum_reg))
                                                                      (set! pc parse))
                                                                    (if (and (= (length datum_reg) 3)
                                                                             (catch? (caddr datum_reg)))
                                                                        (begin
                                                                          (set! k_reg
                                                                            (make-cont '<cont-24> datum_reg handler_reg k_reg))
                                                                          (set! datum_reg (try-body datum_reg))
                                                                          (set! pc parse))
                                                                        (if (and (= (length datum_reg) 3)
                                                                                 (finally? (caddr datum_reg)))
                                                                            (begin
                                                                              (set! k_reg
                                                                                (make-cont '<cont-22> datum_reg handler_reg k_reg))
                                                                              (set! datum_reg (try-body datum_reg))
                                                                              (set! pc parse))
                                                                            (if (and (= (length datum_reg) 4)
                                                                                     (catch? (caddr datum_reg))
                                                                                     (finally? (cadddr datum_reg)))
                                                                                (begin
                                                                                  (set! k_reg
                                                                                    (make-cont '<cont-20> datum_reg handler_reg k_reg))
                                                                                  (set! datum_reg (try-body datum_reg))
                                                                                  (set! pc parse))
                                                                                (begin
                                                                                  (set! exception_reg (format "bad try syntax: ~a" datum_reg))
                                                                                  (set! pc apply-handler))))))
                                                                (if (raise? datum_reg)
                                                                    (begin
                                                                      (set! k_reg (make-cont '<cont-17> k_reg))
                                                                      (set! datum_reg (cadr datum_reg))
                                                                      (set! pc parse))
                                                                    (if (dict? datum_reg)
                                                                        (begin
                                                                          (set! k_reg (make-cont '<cont-16> k_reg))
                                                                          (set! pairs_reg (cdr datum_reg))
                                                                          (set! pc parse-pairs))
                                                                        (if (application? datum_reg)
                                                                            (begin
                                                                              (set! k_reg
                                                                                (make-cont '<cont-15> datum_reg handler_reg k_reg))
                                                                              (set! datum_reg (car datum_reg))
                                                                              (set! pc parse))
                                                                            (begin
                                                                              (set! exception_reg
                                                                                (format "bad concrete syntax: ~a" datum_reg))
                                                                              (set! pc apply-handler))))))))))))))))))))))

(define*
  parse-pairs
  (lambda ()
    (if (null? pairs_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-39> pairs_reg handler_reg k_reg))
          (set! datum_reg (caar pairs_reg))
          (set! pc parse)))))

(define*
  parse-all
  (lambda ()
    (if (null? datum-list_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-41> datum-list_reg handler_reg k_reg))
          (set! datum_reg (car datum-list_reg))
          (set! pc parse)))))

(define*
  expand-quasiquote
  (lambda ()
    (if (vector? datum_reg)
        (begin
          (set! k_reg (make-cont '<cont-46> k_reg))
          (set! datum_reg (vector->list datum_reg))
          (set! pc expand-quasiquote))
        (if (not (pair? datum_reg))
            (begin
              (set! value_reg (list 'quote datum_reg))
              (set! pc apply-cont))
            (if (quasiquote? datum_reg)
                (begin
                  (set! value_reg (list 'quote datum_reg))
                  (set! pc apply-cont))
                (if (unquote? datum_reg)
                    (begin
                      (set! value_reg (cadr datum_reg))
                      (set! pc apply-cont))
                    (if (unquote-splicing? (car datum_reg))
                        (if (null? (cdr datum_reg))
                            (begin
                              (set! value_reg (cadr (car datum_reg)))
                              (set! pc apply-cont))
                            (begin
                              (set! k_reg (make-cont '<cont-45> datum_reg k_reg))
                              (set! datum_reg (cdr datum_reg))
                              (set! pc expand-quasiquote)))
                        (if (quasiquote-list? datum_reg)
                            (begin
                              (set! k_reg (make-cont '<cont-44> k_reg))
                              (set! pc expand-quasiquote-list))
                            (begin
                              (set! k_reg
                                (make-cont '<cont-43> datum_reg handler_reg k_reg))
                              (set! datum_reg (car datum_reg))
                              (set! pc expand-quasiquote))))))))))

(define*
  expand-quasiquote-list
  (lambda ()
    (if (null? datum_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-48> datum_reg handler_reg k_reg))
          (set! datum_reg (car datum_reg))
          (set! pc expand-quasiquote)))))

(define quasiquote-list?
  (lambda (datum)
    (return*
      (or (null? datum)
          (and (pair? datum)
               (not (quasiquote? datum))
               (not (unquote? datum))
               (not (unquote-splicing? datum))
               (not (quasiquote? (car datum)))
               (not (unquote-splicing? (car datum)))
               (quasiquote-list? (cdr datum)))))))

(define head
  (lambda (formals)
    (if (symbol? formals)
        (return* '())
        (if (pair? (cdr formals))
            (return* (cons (car formals) (head (cdr formals))))
            (return* (list (car formals)))))))

(define last
  (lambda (formals)
    (if (symbol? formals)
        (return* formals)
        (if (pair? (cdr formals))
            (return* (last (cdr formals)))
            (return* (cdr formals))))))

(define mit-style?
  (lambda (datum) (return* (not (symbol? (cadr datum))))))

(define literal?
  (lambda (datum)
    (return*
      (or (number? datum)
          (boolean? datum)
          (char? datum)
          (string? datum)
          (vector? datum)))))

(define anything? (lambda (datum) (return* #t)))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (return*
        (and (list? datum)
             (op (length datum) len)
             (eq? (car datum) tag))))))

(define application?
  (lambda (datum)
    (return*
      (and (list? datum)
           (not (null? datum))
           (not (reserved-keyword? (car datum)))))))

(define reserved-keyword?
  (lambda (x)
    (return*
      (and (symbol? x)
           (memq
             x
             (list 'quote 'quasiquote 'lambda 'if 'set! 'define 'begin
              'cond 'and 'or 'let 'let* 'letrec 'case 'record-case 'try
              'catch 'finally 'raise 'dict))))))

(define try-body (lambda (x) (return* (cadr x))))

(define catch-var (lambda (x) (return* (cadr x))))

(define catch-exps (lambda (x) (return* (cddr x))))

(define finally-exps (lambda (x) (return* (cdr x))))

(define print-parsed-sexps
  (lambda (filename)
    (return*
      (for-each pretty-print (get-parsed-sexps filename)))))

(define get-parsed-sexps
  (lambda (filename)
    (set! k_reg (make-cont '<cont-49>))
    (set! handler_reg init-handler)
    (set! input_reg (read-content filename))
    (set! pc scan-input)))

(define*
  parse-sexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg (make-cont2 '<cont2-19> handler_reg k_reg))
          (set! pc read-sexp)))))

(define testall
  (lambda ()
    (set! k_reg (make-cont2 '<cont2-22>))
    (set! handler_reg REP-handler)
    (set! input_reg "(load \"examples.ss\")")
    (set! pc read-datum)))

(define start (lambda () (set! pc read-eval-print)))

(define pretty-print-prim
  (lambda (arg) (set! *need-newline* #f) (pretty-print arg)))

(define newline-prim
  (lambda () (set! *need-newline* #f) (return* (newline))))

(define display-prim
  (lambda (arg)
    (let ((s 'undefined) (len 'undefined))
      (set! s (format "~s" arg))
      (set! len (string-length s))
      (set! *need-newline*
        (true? (not (equal? (substring s (- len 1) len) "\n"))))
      (return* (display s)))))

(define read-line
  (lambda (prompt) (printf prompt) (return* (read))))

(define*
  read-eval-print
  (lambda ()
    (let ((input 'undefined) (input-string 'undefined))
      (set! input (read-line "==> "))
      (set! input-string (format "~s" input))
      (set! k_reg (make-cont2 '<cont2-20>))
      (set! handler_reg REP-handler)
      (set! input_reg input-string)
      (set! pc read-datum))))

(define*
  m
  (lambda ()
    (if (eq? (car exp_reg) 'lit-exp)
        (let ((datum 'undefined))
          (set! datum (list-ref exp_reg 1))
          (set! value_reg datum)
          (set! pc apply-cont))
        (if (eq? (car exp_reg) 'var-exp)
            (let ((id 'undefined))
              (set! id (list-ref exp_reg 1))
              (set! variable_reg id)
              (set! pc lookup-value))
            (if (eq? (car exp_reg) 'if-exp)
                (let ((test-exp 'undefined)
                      (then-exp 'undefined)
                      (else-exp 'undefined))
                  (set! else-exp (list-ref exp_reg 3))
                  (set! then-exp (list-ref exp_reg 2))
                  (set! test-exp (list-ref exp_reg 1))
                  (set! k_reg
                    (make-cont '<cont-70> else-exp then-exp env_reg handler_reg
                      k_reg))
                  (set! exp_reg test-exp)
                  (set! pc m))
                (if (eq? (car exp_reg) 'assign-exp)
                    (let ((var 'undefined) (rhs-exp 'undefined))
                      (set! rhs-exp (list-ref exp_reg 2))
                      (set! var (list-ref exp_reg 1))
                      (set! k_reg
                        (make-cont '<cont-69> var env_reg handler_reg k_reg))
                      (set! exp_reg rhs-exp)
                      (set! pc m))
                    (if (eq? (car exp_reg) 'define-exp)
                        (let ((var 'undefined) (rhs-exp 'undefined))
                          (set! rhs-exp (list-ref exp_reg 2))
                          (set! var (list-ref exp_reg 1))
                          (if (= (length rhs-exp) 1)
                              (begin
                                (set! k_reg
                                  (make-cont '<cont-68> var env_reg handler_reg k_reg))
                                (set! exp_reg (car rhs-exp))
                                (set! pc m))
                              (begin
                                (set! k_reg
                                  (make-cont '<cont-66> rhs-exp var env_reg handler_reg
                                    k_reg))
                                (set! exp_reg (cadr rhs-exp))
                                (set! pc m))))
                        (if (eq? (car exp_reg) 'define-syntax-exp)
                            (let ((keyword 'undefined) (clauses 'undefined))
                              (set! clauses (list-ref exp_reg 2))
                              (set! keyword (list-ref exp_reg 1))
                              (set! k_reg (make-cont '<cont-63> clauses k_reg))
                              (set! env_reg macro-env)
                              (set! var_reg keyword)
                              (set! pc lookup-binding-in-first-frame))
                            (if (eq? (car exp_reg) 'begin-exp)
                                (let ((exps 'undefined))
                                  (set! exps (list-ref exp_reg 1))
                                  (set! exps_reg exps)
                                  (set! pc eval-sequence))
                                (if (eq? (car exp_reg) 'lambda-exp)
                                    (let ((formals 'undefined) (body 'undefined))
                                      (set! body (list-ref exp_reg 2))
                                      (set! formals (list-ref exp_reg 1))
                                      (set! value_reg (closure formals body env_reg))
                                      (set! pc apply-cont))
                                    (if (eq? (car exp_reg) 'mu-lambda-exp)
                                        (let ((formals 'undefined)
                                              (runt 'undefined)
                                              (body 'undefined))
                                          (set! body (list-ref exp_reg 3))
                                          (set! runt (list-ref exp_reg 2))
                                          (set! formals (list-ref exp_reg 1))
                                          (set! value_reg (mu-closure formals runt body env_reg))
                                          (set! pc apply-cont))
                                        (if (eq? (car exp_reg) 'try-catch-exp)
                                            (let ((body 'undefined)
                                                  (cvar 'undefined)
                                                  (cexps 'undefined))
                                              (set! cexps (list-ref exp_reg 3))
                                              (set! cvar (list-ref exp_reg 2))
                                              (set! body (list-ref exp_reg 1))
                                              (let ((new-handler 'undefined))
                                                (set! new-handler
                                                  (try-catch-handler cvar cexps env_reg handler_reg k_reg))
                                                (set! handler_reg new-handler)
                                                (set! exp_reg body)
                                                (set! pc m)))
                                            (if (eq? (car exp_reg) 'try-finally-exp)
                                                (let ((body 'undefined) (fexps 'undefined))
                                                  (set! fexps (list-ref exp_reg 2))
                                                  (set! body (list-ref exp_reg 1))
                                                  (let ((new-handler 'undefined))
                                                    (set! new-handler
                                                      (try-finally-handler fexps env_reg handler_reg))
                                                    (set! k_reg
                                                      (make-cont '<cont-62> fexps env_reg handler_reg k_reg))
                                                    (set! handler_reg new-handler)
                                                    (set! exp_reg body)
                                                    (set! pc m)))
                                                (if (eq? (car exp_reg) 'try-catch-finally-exp)
                                                    (let ((body 'undefined)
                                                          (cvar 'undefined)
                                                          (cexps 'undefined)
                                                          (fexps 'undefined))
                                                      (set! fexps (list-ref exp_reg 4))
                                                      (set! cexps (list-ref exp_reg 3))
                                                      (set! cvar (list-ref exp_reg 2))
                                                      (set! body (list-ref exp_reg 1))
                                                      (let ((new-handler 'undefined))
                                                        (set! new-handler
                                                          (try-catch-finally-handler cvar cexps fexps env_reg
                                                            handler_reg k_reg))
                                                        (set! k_reg
                                                          (make-cont '<cont-62> fexps env_reg handler_reg k_reg))
                                                        (set! handler_reg new-handler)
                                                        (set! exp_reg body)
                                                        (set! pc m)))
                                                    (if (eq? (car exp_reg) 'raise-exp)
                                                        (let ((exp 'undefined))
                                                          (set! exp (list-ref exp_reg 1))
                                                          (set! k_reg (make-cont '<cont-60> handler_reg))
                                                          (set! exp_reg exp)
                                                          (set! pc m))
                                                        (if (eq? (car exp_reg) 'dict-exp)
                                                            (let ((pairs 'undefined))
                                                              (set! pairs (list-ref exp_reg 1))
                                                              (set! value_reg (list 'dict pairs))
                                                              (set! pc apply-cont))
                                                            (if (eq? (car exp_reg) 'app-exp)
                                                                (let ((operator 'undefined) (operands 'undefined))
                                                                  (set! operands (list-ref exp_reg 2))
                                                                  (set! operator (list-ref exp_reg 1))
                                                                  (set! k_reg
                                                                    (make-cont '<cont-59> operator env_reg handler_reg k_reg))
                                                                  (set! exps_reg operands)
                                                                  (set! pc m*))
                                                                (error 'm
                                                                  "bad abstract syntax: ~a"
                                                                  exp_reg))))))))))))))))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (return*
      (make-handler '<handler-3> cexps cvar env handler k))))

(define try-finally-handler
  (lambda (fexps env handler)
    (return* (make-handler '<handler-4> fexps env handler))))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (return*
      (make-handler '<handler-5> cexps cvar fexps env handler
        k))))

(define closure
  (lambda (formals body env)
    (return* (make-proc '<proc-1> formals body env))))

(define mu-closure
  (lambda (formals runt body env)
    (return* (make-proc '<proc-2> formals runt body env))))

(define*
  m*
  (lambda ()
    (if (null? exps_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-72> exps_reg env_reg handler_reg k_reg))
          (set! exp_reg (car exps_reg))
          (set! pc m)))))

(define*
  eval-sequence
  (lambda ()
    (set! k_reg
      (make-cont '<cont-73> exps_reg env_reg handler_reg k_reg))
    (set! exp_reg (car exps_reg))
    (set! pc m)))

(define make-initial-env-extended
  (lambda (env) (return* env)))

(define make-toplevel-env
  (lambda ()
    (return*
      (make-initial-env-extended
        (make-initial-environment
          (list 'exit 'eval 'parse 'parse-string 'apply 'sqrt 'print
           'display 'newline 'load 'null? 'cons 'car 'cdr 'list '+ '-
           '* '/ '< '> '= 'equal? 'eq? 'memq 'range 'set-car! 'set-cdr!
           'import 'get 'call-with-current-continuation 'call/cc
           'reverse 'append 'list->vector 'dir 'current-time 'map
           'for-each 'env 'using 'not 'printf 'vector 'vector-set!
           'vector-ref 'make-vector 'help)
          (list (make-proc '<proc-49>) (make-proc '<proc-48>)
           (make-proc '<proc-47>) (make-proc '<proc-46>)
           (make-proc '<proc-45>) (make-proc '<proc-44>)
           (make-proc '<proc-43>) (make-proc '<proc-42>)
           (make-proc '<proc-41>) (make-proc '<proc-40>)
           (make-proc '<proc-39>) (make-proc '<proc-38>)
           (make-proc '<proc-37>) (make-proc '<proc-36>)
           (make-proc '<proc-35>) (make-proc '<proc-34>)
           (make-proc '<proc-33>) (make-proc '<proc-32>)
           (make-proc '<proc-31>) (make-proc '<proc-30>)
           (make-proc '<proc-29>) (make-proc '<proc-28>)
           (make-proc '<proc-27>) (make-proc '<proc-26>)
           (make-proc '<proc-25>) (make-proc '<proc-24>)
           (make-proc '<proc-23>) (make-proc '<proc-22>)
           (make-proc '<proc-21>) (make-proc '<proc-20>)
           (make-proc '<proc-19>) (make-proc '<proc-19>)
           (make-proc '<proc-18>) (make-proc '<proc-17>)
           (make-proc '<proc-16>) (make-proc '<proc-15>)
           (make-proc '<proc-14>) (make-proc '<proc-13>)
           (make-proc '<proc-12>) (make-proc '<proc-11>)
           (make-proc '<proc-10>) (make-proc '<proc-9>)
           (make-proc '<proc-8>) (make-proc '<proc-7>)
           (make-proc '<proc-6>) (make-proc '<proc-5>)
           (make-proc '<proc-4>) (make-proc '<proc-3>)))))))

(define*
  map-prim
  (lambda ()
    (let ((len 'undefined) (list-args 'undefined))
      (set! list-args (listify args_reg))
      (set! len (length args_reg))
      (if (= len 1)
          (begin (set! list1_reg (car list-args)) (set! pc map1))
          (if (= len 2)
              (begin
                (set! list2_reg (cadr list-args))
                (set! list1_reg (car list-args))
                (set! pc map2))
              (begin (set! lists_reg list-args) (set! pc mapN)))))))

(define listify
  (lambda (arg-list)
    (if (null? arg-list)
        (return* '())
        (if (list? (car arg-list))
            (return* (cons (car arg-list) (listify (cdr arg-list))))
            (if (vector? (car arg-list))
                (return*
                  (cons
                    (my-vector->list (car arg-list))
                    (listify (cdr arg-list))))
                (if (string? (car arg-list))
                    (return*
                      (cons
                        (string->list (car arg-list))
                        (listify (cdr arg-list))))
                    (error 'map
                      "cannot use object type '~a' in map"
                      (get_type (car arg-list)))))))))

(define*
  map1
  (lambda ()
    (if (null? list1_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (if (dlr-exp? proc_reg)
            (begin
              (set! k_reg (make-cont '<cont-76> list1_reg proc_reg k_reg))
              (set! list1_reg (cdr list1_reg))
              (set! pc map1))
            (begin
              (set! k2_reg
                (make-cont '<cont-75> list1_reg proc_reg env_reg handler_reg
                  k_reg))
              (set! env2_reg env_reg)
              (set! args_reg (list (car list1_reg)))
              (set! pc apply-proc))))))

(define*
  map2
  (lambda ()
    (if (null? list1_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (if (dlr-exp? proc_reg)
            (begin
              (set! k_reg
                (make-cont '<cont-78> list1_reg list2_reg proc_reg k_reg))
              (set! list2_reg (cdr list2_reg))
              (set! list1_reg (cdr list1_reg))
              (set! pc map2))
            (begin
              (set! k2_reg
                (make-cont '<cont-77> list1_reg list2_reg proc_reg env_reg
                  handler_reg k_reg))
              (set! env2_reg env_reg)
              (set! args_reg (list (car list1_reg) (car list2_reg)))
              (set! pc apply-proc))))))

(define*
  mapN
  (lambda ()
    (if (null? (car lists_reg))
        (begin (set! value_reg '()) (set! pc apply-cont))
        (if (dlr-exp? proc_reg)
            (begin
              (set! k_reg (make-cont '<cont-80> lists_reg proc_reg k_reg))
              (set! lists_reg (map cdr lists_reg))
              (set! pc mapN))
            (begin
              (set! k2_reg
                (make-cont '<cont-79> lists_reg proc_reg env_reg handler_reg
                  k_reg))
              (set! env2_reg env_reg)
              (set! args_reg (map car lists_reg))
              (set! pc apply-proc))))))

(define*
  for-each-prim
  (lambda ()
    (let ((arg-list 'undefined))
      (set! arg-list (listify lists_reg))
      (if (null? (car arg-list))
          (begin (set! value_reg '<void>) (set! pc apply-cont))
          (if (dlr-exp? proc_reg)
              (begin
                (dlr-apply proc_reg (map car arg-list))
                (set! lists_reg (map cdr arg-list))
                (set! pc for-each-prim))
              (begin
                (set! k2_reg
                  (make-cont '<cont-81> arg-list proc_reg env_reg handler_reg
                    k_reg))
                (set! env2_reg env_reg)
                (set! args_reg (map car arg-list))
                (set! pc apply-proc)))))))

(define get-current-time
  (lambda ()
    (let ((now 'undefined))
      (set! now (current-time))
      (return*
        (+ (time-second now)
           (inexact (/ (time-nanosecond now) 1000000000)))))))

(define*
  get-primitive
  (lambda ()
    (let ((sym 'undefined))
      (set! sym (car args_reg))
      (set! k_reg
        (make-cont '<cont-82> args_reg sym handler_reg k_reg))
      (set! variable_reg sym)
      (set! pc lookup-value))))

(define*
  import-primitive
  (lambda ()
    (let ((filename 'undefined))
      (set! filename (car args_reg))
      (if (null? (cdr args_reg))
          (begin (set! filename_reg filename) (set! pc load-file))
          (let ((module-name 'undefined))
            (set! module-name (cadr args_reg))
            (set! k_reg
              (make-cont '<cont-83> filename env_reg handler_reg k_reg))
            (set! var_reg module-name)
            (set! pc lookup-binding-in-first-frame))))))

(define*
  call/cc-primitive
  (lambda ()
    (let ((fake-k 'undefined))
      (set! fake-k (make-proc '<proc-50> k_reg))
      (if (dlr-exp? proc_reg)
          (begin
            (set! value_reg (dlr-apply proc_reg (list fake-k)))
            (set! pc apply-cont))
          (begin
            (set! k2_reg k_reg)
            (set! env2_reg env_reg)
            (set! args_reg (list fake-k))
            (set! pc apply-proc))))))

(define flatten
  (lambda (lists)
    (if (null? lists)
        (return* '())
        (if (list? (car lists))
            (return*
              (append (flatten (car lists)) (flatten (cdr lists))))
            (return* (cons (car lists) (flatten (cdr lists))))))))

(define dir
  (lambda (args env)
    (return*
      (sort
        symbol<?
        (if (null? args)
            (flatten
              (append
                (map get-variables-from-frame (frames macro-env))
                (map get-variables-from-frame (frames env))))
            (get-variables-from-frame (car (frames (car args)))))))))

(define get-variables-from-frame
  (lambda (frame) (return* (map binding-variable frame))))

(define symbol<?
  (lambda (a b)
    (let ((a_string 'undefined) (b_string 'undefined))
      (set! b_string (symbol->string b))
      (set! a_string (symbol->string a))
      (return* (string<? a_string b_string)))))

(define*
  load-file
  (lambda ()
    (if (member filename_reg load-stack)
        (begin
          (printf "skipping recursive load of ~a~%" filename_reg)
          (set! value_reg '<void>)
          (set! pc apply-cont))
        (if (not (string? filename_reg))
            (begin
              (set! exception_reg
                (format "filename is not a string: ~a" filename_reg))
              (set! pc apply-handler))
            (if (not (file-exists? filename_reg))
                (begin
                  (set! exception_reg
                    (format "file does not exist: ~a" filename_reg))
                  (set! pc apply-handler))
                (begin
                  (set! load-stack (cons filename_reg load-stack))
                  (set! k_reg
                    (make-cont '<cont-85> env_reg handler_reg k_reg))
                  (set! input_reg (read-content filename_reg))
                  (set! pc scan-input)))))))

(define*
  load-loop
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
        (begin (set! value_reg '<void>) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont2 '<cont2-24> env_reg handler_reg k_reg))
          (set! pc read-sexp)))))

(define*
  load-files
  (lambda ()
    (if (null? filenames_reg)
        (begin (set! value_reg 'ok) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-88> filenames_reg env_reg handler_reg
              k_reg))
          (set! filename_reg (car filenames_reg))
          (set! pc load-file)))))

(define*
  help-prim
  (lambda ()
    (set! k_reg (make-cont '<cont-89> k_reg))
    (set! variable_reg var_reg)
    (set! pc lookup-binding)))

(define range
  (lambda args
    (let ((range 'undefined))
      (set! range
        (lambda (n end step acc)
          (if (>= n end)
              (reverse acc)
              (range (+ n step) end step (cons n acc)))))
      (if (null? (cdr args))
          (return* (range 0 (car args) 1 '()))
          (if (null? (cddr args))
              (return* (range (car args) (cadr args) 1 '()))
              (return*
                (range (car args) (cadr args) (caddr args) '())))))))

(define make-external-proc
  (lambda (external-function-object)
    (return* (make-proc '<proc-51> external-function-object))))

(define Main
  (lambda (args)
    (printf "Pyjama Scheme (0.1)\n")
    (printf "(c) 2009, IPRE\n")
    (set! k_reg REP-k)
    (set! handler_reg REP-handler)
    (set! env_reg toplevel-env)
    (set! filenames_reg (list args))
    (set! pc load-files)
    (return* (trampoline))))

(define execute
  (lambda (input-string)
    (set! k_reg (make-cont2 '<cont2-25>))
    (set! handler_reg init-handler)
    (set! input_reg input-string)
    (set! pc read-datum)
    (return* (trampoline))))

(define pattern?
  (lambda (x)
    (return*
      (or (null? x)
          (number? x)
          (boolean? x)
          (symbol? x)
          (and (pair? x) (pattern? (car x)) (pattern? (cdr x)))))))

(define pattern-variable?
  (lambda (x)
    (return*
      (and (symbol? x)
           (equal? "?" (substring (symbol->string x) 0 1))))))

(define constant?
  (lambda (x)
    (return*
      (and (not (pattern-variable? x)) (not (pair? x))))))

(define*
  occurs?
  (lambda ()
    (if (constant? pattern_reg)
        (begin (set! value_reg #f) (set! pc apply-cont))
        (if (pattern-variable? pattern_reg)
            (begin
              (set! value_reg (equal? var_reg pattern_reg))
              (set! pc apply-cont))
            (begin
              (set! k_reg
                (make-cont '<cont-91> pattern_reg var_reg k_reg))
              (set! pattern_reg (car pattern_reg))
              (set! pc occurs?))))))

(define*
  unify-patterns
  (lambda ()
    (if (pattern-variable? p1_reg)
        (if (pattern-variable? p2_reg)
            (begin
              (set! value_reg (make-sub 'unit p1_reg p2_reg))
              (set! pc apply-cont))
            (begin
              (set! k_reg (make-cont '<cont-92> p1_reg p2_reg k_reg))
              (set! pattern_reg p2_reg)
              (set! var_reg p1_reg)
              (set! pc occurs?)))
        (if (pattern-variable? p2_reg)
            (begin
              (set! temp_1 p2_reg)
              (set! temp_2 p1_reg)
              (set! p1_reg temp_1)
              (set! p2_reg temp_2)
              (set! pc unify-patterns))
            (if (and (constant? p1_reg)
                     (constant? p2_reg)
                     (equal? p1_reg p2_reg))
                (begin
                  (set! value_reg (make-sub 'empty))
                  (set! pc apply-cont))
                (if (and (pair? p1_reg) (pair? p2_reg))
                    (begin
                      (set! pair2_reg p2_reg)
                      (set! pair1_reg p1_reg)
                      (set! pc unify-pairs))
                    (begin (set! value_reg #f) (set! pc apply-cont))))))))

(define*
  unify-pairs
  (lambda ()
    (set! k_reg
      (make-cont '<cont-96> pair1_reg pair2_reg k_reg))
    (set! p2_reg (car pair2_reg))
    (set! p1_reg (car pair1_reg))
    (set! pc unify-patterns)))

(define*
  instantiate
  (lambda ()
    (if (constant? pattern_reg)
        (begin (set! value_reg pattern_reg) (set! pc apply-cont))
        (if (pattern-variable? pattern_reg)
            (begin (set! var_reg pattern_reg) (set! pc apply-sub))
            (if (pair? pattern_reg)
                (begin
                  (set! k_reg (make-cont '<cont-97> pattern_reg s_reg k_reg))
                  (set! pattern_reg (car pattern_reg))
                  (set! pc instantiate))
                (error 'instantiate "bad pattern: ~a" pattern_reg))))))

(define make-sub
  (lambda args (return* (cons 'substitution args))))

(define*
  apply-sub
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr s_reg))
      (if (eq? (car temp_1) 'empty)
          (begin (set! value_reg var_reg) (set! pc apply-cont))
          (if (eq? (car temp_1) 'unit)
              (let ((new-var 'undefined) (new-pattern 'undefined))
                (set! new-pattern (list-ref temp_1 2))
                (set! new-var (list-ref temp_1 1))
                (if (equal? var_reg new-var)
                    (begin (set! value_reg new-pattern) (set! pc apply-cont))
                    (begin (set! value_reg var_reg) (set! pc apply-cont))))
              (if (eq? (car temp_1) 'composite)
                  (let ((s1 'undefined) (s2 'undefined))
                    (set! s2 (list-ref temp_1 2))
                    (set! s1 (list-ref temp_1 1))
                    (set! k_reg (make-cont '<cont-98> s2 k_reg))
                    (set! s_reg s1)
                    (set! pc apply-sub))
                  (error 'apply-sub "bad substitution: ~a" s_reg)))))))

(define chars-to-scan 'undefined)

(define read-line-count 1)

(define read-char-count 0)

(define init-cont (make-cont '<cont-2>))

(define init-cont2 (make-cont2 '<cont2-2>))

(define init-handler (make-handler '<handler-1>))

(define mit-define-transformer (make-macro '<macro-1>))

(define and-transformer (make-macro '<macro-2>))

(define or-transformer (make-macro '<macro-3>))

(define cond-transformer (make-macro '<macro-4>))

(define let-transformer (make-macro '<macro-5>))

(define letrec-transformer (make-macro '<macro-6>))

(define let*-transformer (make-macro '<macro-7>))

(define case-transformer (make-macro '<macro-8>))

(define record-case-transformer (make-macro '<macro-9>))

(define macro-env (make-macro-env))

(define quote? (tagged-list 'quote = 2))

(define quasiquote? (tagged-list 'quasiquote = 2))

(define unquote? (tagged-list 'unquote = 2))

(define unquote-splicing?
  (tagged-list 'unquote-splicing = 2))

(define if-then? (tagged-list 'if = 3))

(define if-else? (tagged-list 'if = 4))

(define assignment? (tagged-list 'set! = 3))

(define define? (tagged-list 'define >= 3))

(define define-syntax? (tagged-list 'define-syntax >= 3))

(define begin? (tagged-list 'begin >= 2))

(define lambda? (tagged-list 'lambda >= 3))

(define raise? (tagged-list 'raise = 2))

(define dict? (tagged-list 'dict >= 1))

(define try? (tagged-list 'try >= 2))

(define catch? (tagged-list 'catch >= 3))

(define finally? (tagged-list 'finally >= 2))

(define *need-newline* #f)

(define REP-k (make-cont '<cont-57>))

(define REP-handler (make-handler '<handler-2>))

(define load-stack '())

(define make-vector list->vector)

(define toplevel-env (make-toplevel-env))

(define macro-env (make-macro-env))

;; the trampoline
(define trampoline
  (lambda () (if pc (begin (pc) (trampoline)) final_reg)))

(define run
  (lambda (setup . args) (apply setup args) (trampoline)))

