(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; EOPL support

(load "petite-init.ss")
(load "define-datatype.ss")

(define-datatype expression expression?
  (lit-exp (datum anything?)) (var-exp (id symbol?))
  (if-exp
    (test-exp expression?)
    (then-exp expression?)
    (else-exp expression?))
  (assign-exp (var symbol?) (rhs-exp expression?))
  (define-exp (id symbol?) (rhs-exp (list-of expression?)))
  (define-syntax-exp
    (keyword symbol?)
    (clauses (list-of (list-of pattern?))))
  (begin-exp (exps (list-of expression?)))
  (lambda-exp (formals (list-of symbol?)) (body expression?))
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
  (raise-exp (exp expression?))
  (dict-exp (pairs (list-of (list-of expression?)))))

;;----------------------------------------------------------------------
;; continuation datatype

(define make-cont (lambda args (cons 'continuation args)))

(define*
  apply-cont
  (lambda (k value)
    (record-case (cdr k)
      (<cont-1> (token k) (apply-cont k (cons token value)))
      (<cont-2> () (halt* value))
      (<cont-3> (chars k)
       (apply-cont2
         k
         (append value (list read-line-count read-char-count))
         chars))
      (<cont-4> (handler k)
       (read-sexp value handler (make-cont2 '<cont2-3> handler k)))
      (<cont-5> ()
       (print-unparsed-sexps value init-handler init-cont))
      (<cont-6> (k) (apply-cont k (binding-value value)))
      (<cont-7> (variable env handler k)
       (if value
           (lookup-variable-components value "" env handler k)
           (if (dlr-env-contains variable)
               (apply-cont k (dlr-env-lookup variable))
               (apply-handler
                 handler
                 (format "unbound variable ~a" variable)))))
      (<cont-8> (components path var handler k)
       (if (null? (cdr components))
           (apply-cont k value)
           (let ((result (binding-value value))
                 (new-path (if (string=? path "")
                               (format "~a" var)
                               (format "~a.~a" path var))))
             (if (not (environment? result))
                 (apply-handler
                   handler
                   (format "~a is not a module" new-path))
                 (lookup-variable-components (cdr components) new-path
                   result handler k)))))
      (<cont-9> (datum handler k)
       (if (pattern-macro? value)
           (process-macro-clauses
             (macro-clauses value)
             datum
             handler
             k)
           (apply-macro value datum k)))
      (<cont-10> (clauses datum right-pattern handler k)
       (if value
           (instantiate right-pattern value k)
           (process-macro-clauses (cdr clauses) datum handler k)))
      (<cont-11> (bindings k)
       (apply-cont k `(let (,(car bindings)) ,value)))
      (<cont-12> (k) (apply-cont k `(cond ,@value)))
      (<cont-13> (clauses var k)
       (let ((clause (car clauses)))
         (cond
           ((eq? (car clause) 'else)
            (apply-cont k (cons clause value)))
           ((symbol? (car clause))
            (apply-cont
              k
              (cons `((eq? ,var ',(car clause)) ,@(cdr clause)) value)))
           (else
            (apply-cont
              k
              (cons
                `((memq ,var ',(car clause)) ,@(cdr clause))
                value))))))
      (<cont-14> (v1 k) (apply-cont k (app-exp v1 value)))
      (<cont-15> (datum handler k)
       (parse-all
         (cdr datum)
         handler
         (make-cont '<cont-14> value k)))
      (<cont-16> (k) (apply-cont k (dict-exp value)))
      (<cont-17> (k) (apply-cont k (raise-exp value)))
      (<cont-18> (cexps datum body k)
       (let ((cvar (catch-var (caddr datum))))
         (apply-cont
           k
           (try-catch-finally-exp body cvar cexps value))))
      (<cont-19> (datum body handler k)
       (parse-all
         (finally-exps (cadddr datum))
         handler
         (make-cont '<cont-18> value datum body k)))
      (<cont-20> (datum handler k)
       (parse-all
         (catch-exps (caddr datum))
         handler
         (make-cont '<cont-19> datum value handler k)))
      (<cont-21> (body k)
       (apply-cont k (try-finally-exp body value)))
      (<cont-22> (datum handler k)
       (parse-all
         (finally-exps (caddr datum))
         handler
         (make-cont '<cont-21> value k)))
      (<cont-23> (datum body k)
       (let ((cvar (catch-var (caddr datum))))
         (apply-cont k (try-catch-exp body cvar value))))
      (<cont-24> (datum handler k)
       (parse-all
         (catch-exps (caddr datum))
         handler
         (make-cont '<cont-23> datum value k)))
      (<cont-25> (datum k)
       (if (list? (cadr datum))
           (apply-cont k (lambda-exp (cadr datum) value))
           (apply-cont
             k
             (mu-lambda-exp
               (head (cadr datum))
               (last (cadr datum))
               value))))
      (<cont-26> (datum handler k)
       (cond
         ((null? value)
          (apply-handler
            handler
            (format "bad concrete syntax: ~a" datum)))
         ((null? (cdr value)) (apply-cont k (car value)))
         (else (apply-cont k (begin-exp value)))))
      (<cont-27> (datum body k)
       (apply-cont k (define-exp (cadr datum) (list value body))))
      (<cont-28> (datum handler k)
       (parse
         (caddr datum)
         handler
         (make-cont '<cont-27> datum value k)))
      (<cont-29> (datum k)
       (apply-cont k (define-exp (cadr datum) (list value))))
      (<cont-30> (handler k) (parse value handler k))
      (<cont-31> (datum k)
       (apply-cont k (assign-exp (cadr datum) value)))
      (<cont-32> (v1 v2 k) (apply-cont k (if-exp v1 v2 value)))
      (<cont-33> (datum v1 handler k)
       (parse
         (cadddr datum)
         handler
         (make-cont '<cont-32> v1 value k)))
      (<cont-34> (datum handler k)
       (parse
         (caddr datum)
         handler
         (make-cont '<cont-33> datum value handler k)))
      (<cont-35> (v1 k)
       (apply-cont k (if-exp v1 value (lit-exp #f))))
      (<cont-36> (datum handler k)
       (parse
         (caddr datum)
         handler
         (make-cont '<cont-35> value k)))
      (<cont-37> (a b k) (apply-cont k (cons (list a b) value)))
      (<cont-38> (a pairs handler k)
       (parse-pairs
         (cdr pairs)
         handler
         (make-cont '<cont-37> a value k)))
      (<cont-39> (pairs handler k)
       (parse
         (cadar pairs)
         handler
         (make-cont '<cont-38> value pairs handler k)))
      (<cont-40> (a k) (apply-cont k (cons a value)))
      (<cont-41> (datum-list handler k)
       (parse-all
         (cdr datum-list)
         handler
         (make-cont '<cont-40> value k)))
      (<cont-42> (v1 k) (apply-cont k `(cons ,v1 ,value)))
      (<cont-43> (datum handler k)
       (expand-quasiquote
         (cdr datum)
         handler
         (make-cont '<cont-42> value k)))
      (<cont-44> (k) (apply-cont k `(list ,@value)))
      (<cont-45> (datum k)
       (apply-cont k `(append ,(cadr (car datum)) ,value)))
      (<cont-46> (k) (apply-cont k `(list->vector ,value)))
      (<cont-47> (v1 k) (apply-cont k (cons v1 value)))
      (<cont-48> (datum handler k)
       (expand-quasiquote-list
         (cdr datum)
         handler
         (make-cont '<cont-47> value k)))
      (<cont-49> () (parse-sexps value init-handler init-cont))
      (<cont-50> (exp k) (apply-cont k (cons exp value)))
      (<cont-51> (tokens-left handler k)
       (parse-sexps
         tokens-left
         handler
         (make-cont '<cont-50> value k)))
      (<cont-52> () (m value toplevel-env REP-handler REP-k))
      (<cont-53> ()
       (read-datum "(exit)" REP-handler (make-cont2 '<cont2-20>)))
      (<cont-54> ()
       (m value toplevel-env REP-handler (make-cont '<cont-53>)))
      (<cont-55> ()
       (read-datum
         "(test-all)"
         REP-handler
         (make-cont2 '<cont2-21>)))
      (<cont-56> ()
       (m value toplevel-env REP-handler (make-cont '<cont-55>)))
      (<cont-57> ()
       (if (not (eq? value '<void>)) (pretty-print-prim value))
       (if *need-newline* (newline))
       (read-eval-print))
      (<cont-58> (args env handler k)
       (if (dlr-exp? value)
           (apply-cont k (dlr-apply value args))
           (apply-proc value args env handler k)))
      (<cont-59> (operator env handler k)
       (m operator
          env
          handler
          (make-cont '<cont-58> value env handler k)))
      (<cont-60> (handler) (apply-handler handler value))
      (<cont-61> (v k) (apply-cont k v))
      (<cont-62> (fexps env handler k)
       (eval-sequence
         fexps
         env
         handler
         (make-cont '<cont-61> value k)))
      (<cont-63> (clauses k)
       (set-binding-value! value (make-pattern-macro clauses))
       (apply-cont k '<void>))
      (<cont-64> (docstring rhs-value k)
       (set-binding-docstring! value docstring)
       (set-binding-value! value rhs-value)
       (apply-cont k '<void>))
      (<cont-65> (rhs-value var env handler k)
       (lookup-binding-in-first-frame
         var
         env
         handler
         (make-cont '<cont-64> value rhs-value k)))
      (<cont-66> (rhs-exp var env handler k)
       (m (car rhs-exp)
          env
          handler
          (make-cont '<cont-65> value var env handler k)))
      (<cont-67> (rhs-value k)
       (set-binding-value! value rhs-value)
       (apply-cont k '<void>))
      (<cont-68> (var env handler k)
       (lookup-binding-in-first-frame
         var
         env
         handler
         (make-cont '<cont-67> value k)))
      (<cont-69> (var env handler k)
       (lookup-binding
         var
         env
         handler
         (make-cont '<cont-67> value k)))
      (<cont-70> (else-exp then-exp env handler k)
       (if value
           (m then-exp env handler k)
           (m else-exp env handler k)))
      (<cont-71> (e handler) (apply-handler handler e))
      (<cont-72> (exps env handler k)
       (m* (cdr exps) env handler (make-cont '<cont-47> value k)))
      (<cont-73> (exps env handler k)
       (if (null? (cdr exps))
           (apply-cont k value)
           (eval-sequence (cdr exps) env handler k)))
      (<cont-74> (handler k2) (m value toplevel-env handler k2))
      (<cont-75> (list1 proc env handler k)
       (map1 proc (cdr list1) env handler
         (make-cont '<cont-47> value k)))
      (<cont-76> (list1 proc k)
       (apply-cont
         k
         (cons (dlr-apply proc (list (car list1))) value)))
      (<cont-77> (list1 list2 proc env handler k)
       (map2 proc (cdr list1) (cdr list2) env handler
         (make-cont '<cont-47> value k)))
      (<cont-78> (list1 list2 proc k)
       (apply-cont
         k
         (cons
           (dlr-apply proc (list (car list1) (car list2)))
           value)))
      (<cont-79> (lists proc env handler k)
       (mapN proc (map cdr lists) env handler
         (make-cont '<cont-47> value k)))
      (<cont-80> (lists proc k)
       (apply-cont
         k
         (cons (dlr-apply proc (map car lists)) value)))
      (<cont-81> (arg-list proc env handler k)
       (for-each-prim proc (map cdr arg-list) env handler k))
      (<cont-82> (args sym handler k)
       (cond
         ((null? (cdr args)) (apply-cont k value))
         ((not (environment? value))
          (apply-handler handler (format "~a is not a module" sym)))
         (else (get-primitive (cdr args) value handler k))))
      (<cont-83> (filename env handler k)
       (let ((module (extend env '() '())))
         (set-binding-value! value module)
         (load-file filename module handler k)))
      (<cont-84> (k)
       (set! load-stack (cdr load-stack))
       (apply-cont k value))
      (<cont-85> (env handler k)
       (load-loop value env handler (make-cont '<cont-84> k)))
      (<cont-86> (tokens-left env handler k)
       (load-loop tokens-left env handler k))
      (<cont-87> (tokens-left env handler k)
       (m value
          env
          handler
          (make-cont '<cont-86> tokens-left env handler k)))
      (<cont-88> (filenames env handler k)
       (load-files (cdr filenames) env handler k))
      (<cont-89> (k) (apply-cont k (binding-docstring value)))
      (<cont-90> () (m value toplevel-env init-handler init-cont))
      (<cont-91> (pattern var k)
       (if value (apply-cont k #t) (occurs? var (cdr pattern) k)))
      (<cont-92> (p1 p2 k)
       (if value
           (apply-cont k #f)
           (apply-cont k (make-sub 'unit p1 p2))))
      (<cont-93> (s-car k)
       (if (not value)
           (apply-cont k #f)
           (apply-cont k (make-sub 'composite s-car value))))
      (<cont-94> (new-cdr1 s-car k)
       (unify-patterns
         new-cdr1
         value
         (make-cont '<cont-93> s-car k)))
      (<cont-95> (pair2 s-car k)
       (instantiate
         (cdr pair2)
         s-car
         (make-cont '<cont-94> value s-car k)))
      (<cont-96> (pair1 pair2 k)
       (if (not value)
           (apply-cont k #f)
           (instantiate
             (cdr pair1)
             value
             (make-cont '<cont-95> pair2 value k))))
      (<cont-97> (pattern s k)
       (instantiate
         (cdr pattern)
         s
         (make-cont '<cont-40> value k)))
      (<cont-98> (s2 k) (instantiate value s2 k))
      (else (error 'apply-cont "bad continuation: ~a" k)))))

;;----------------------------------------------------------------------
;; continuation2 datatype

(define make-cont2 (lambda args (cons 'continuation2 args)))

(define*
  apply-cont2
  (lambda (k value1 value2)
    (record-case (cdr k)
      (<cont2-1> (handler k)
       (if (token-type? value1 'end-marker)
           (apply-cont k (list value1))
           (scan-input-loop
             value2
             handler
             (make-cont '<cont-1> value1 k))))
      (<cont2-2> () (halt* value1))
      (<cont2-3> (handler k)
       (if (token-type? (first value2) 'end-marker)
           (apply-cont2 k value1 value2)
           (apply-handler
             handler
             (format
               "tokens left over at line: ~a col: ~a"
               (get-line-count (first value2))
               (get-char-count (first value2))))))
      (<cont2-4> (k) (apply-cont2 k (list->vector value1) value2))
      (<cont2-5> (keyword k)
       (apply-cont2 k (list keyword value1) value2))
      (<cont2-6> (sexp1 k)
       (apply-cont2 k (cons sexp1 value1) value2))
      (<cont2-7> (expected-terminator handler k)
       (read-sexp-sequence
         value2
         expected-terminator
         handler
         (make-cont2 '<cont2-6> value1 k)))
      (<cont2-8> (expected-terminator handler k)
       (close-sexp-sequence value1 value2 expected-terminator
         handler k))
      (<cont2-9> (handler k)
       (read-vector
         value2
         handler
         (make-cont2 '<cont2-6> value1 k)))
      (<cont2-10> (handler k)
       (pretty-print value1)
       (print-unparsed-sexps value2 handler k))
      (<cont2-11> () (halt* (cons value1 value2)))
      (<cont2-12> (bodies k)
       (apply-cont k `(let ,value1 ,@value2 ,@bodies)))
      (<cont2-13> (procs vars k2)
       (apply-cont2
         k2
         (cons `(,(car vars) 'undefined) value1)
         (cons `(set! ,(car vars) ,(car procs)) value2)))
      (<cont2-14> (exp k)
       (apply-cont k `(let ((r ,exp) ,@value1) (cond ,@value2))))
      (<cont2-15> (clauses var k2)
       (let ((clause (car clauses)))
         (if (eq? (car clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(cdr clause))) value1)
               (cons '(else (else-code)) value2))
             (if (symbol? (car clause))
                 (let ((name (car clause)))
                   (apply-cont2
                     k2
                     (cons `(,name (lambda () ,@(cdr clause))) value1)
                     (cons `((eq? ,var ',(car clause)) (,name)) value2)))
                 (let ((name (caar clause)))
                   (apply-cont2
                     k2
                     (cons `(,name (lambda () ,@(cdr clause))) value1)
                     (cons
                       `((memq ,var ',(car clause)) (,name))
                       value2)))))))
      (<cont2-16> (k)
       (apply-cont k `(let ,value1 (cond ,@value2))))
      (<cont2-17> (clauses var k2)
       (let ((clause (car clauses)))
         (if (eq? (car clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(cdr clause))) value1)
               (cons `(else (else-code)) value2))
             (if (symbol? (car clause))
                 (let ((name (car clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda ,(cadr clause) ,@(cddr clause)))
                       value1)
                     (cons
                       `((eq? (car ,var) ',(car clause))
                          (apply ,name (cdr ,var)))
                       value2)))
                 (let ((name (caar clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda ,(cadr clause) ,@(cddr clause)))
                       value1)
                     (cons
                       `((memq (car ,var) ',(car clause))
                          (apply ,name (cdr ,var)))
                       value2)))))))
      (<cont2-18> () (parse value1 init-handler init-cont))
      (<cont2-19> (handler k)
       (parse
         value1
         handler
         (make-cont '<cont-51> value2 handler k)))
      (<cont2-20> ()
       (parse value1 REP-handler (make-cont '<cont-52>)))
      (<cont2-21> ()
       (parse value1 REP-handler (make-cont '<cont-54>)))
      (<cont2-22> ()
       (parse value1 REP-handler (make-cont '<cont-56>)))
      (<cont2-23> (handler k2) (parse value1 handler k2))
      (<cont2-24> (env handler k)
       (parse
         value1
         handler
         (make-cont '<cont-87> value2 env handler k)))
      (<cont2-25> ()
       (parse value1 init-handler (make-cont '<cont-90>)))
      (else (error 'apply-cont2 "bad continuation2: ~a" k)))))

;;----------------------------------------------------------------------
;; handler datatype

(define make-handler (lambda args (cons 'handler args)))

(define*
  apply-handler
  (lambda (handler exception)
    (record-case (cdr handler)
      (<handler-1> () (halt* (list 'exception exception)))
      (<handler-2> ()
       (apply-cont REP-k `(uncaught exception: ,exception)))
      (<handler-3> (cexps cvar env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (eval-sequence cexps new-env handler k)))
      (<handler-4> (fexps env handler)
       (eval-sequence
         fexps
         env
         handler
         (make-cont '<cont-71> exception handler)))
      (<handler-5> (cexps cvar fexps env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (let ((catch-handler (try-finally-handler
                                fexps
                                env
                                handler)))
           (eval-sequence
             cexps
             new-env
             catch-handler
             (make-cont '<cont-62> fexps env handler k)))))
      (else (error 'apply-handler "bad handler: ~a" handler)))))

;;----------------------------------------------------------------------
;; procedure datatype

(define make-proc (lambda args (cons 'procedure args)))

(define*
  apply-proc
  (lambda (proc args env2 handler k2)
    (record-case (cdr proc)
      (<proc-1> (formals body env)
       (if (= (length args) (length formals))
           (m body (extend env formals args) handler k2)
           (apply-handler handler "incorrect number of arguments")))
      (<proc-2> (formals runt body env)
       (if (>= (length args) (length formals))
           (let ((new-env (extend
                            env
                            (cons runt formals)
                            (cons
                              (list-tail args (length formals))
                              (list-head args (length formals))))))
             (m body new-env handler k2))
           (apply-handler handler "not enough arguments given")))
      (<proc-3> () (help-prim (car args) env2 handler k2))
      (<proc-4> () (apply-cont k2 (make-vector-size (car args))))
      (<proc-5> () (apply-cont k2 (apply vector-ref args)))
      (<proc-6> ()
       (apply-cont
         k2
         (vector-set! (car args) (cadr args) (caddr args))))
      (<proc-7> () (apply-cont k2 (make-vector args)))
      (<proc-8> ()
       (apply printf-prim args)
       (apply-cont k2 '<void>))
      (<proc-9> () (apply-cont k2 (not (car args))))
      (<proc-10> () (apply-cont k2 (using-prim args env2)))
      (<proc-11> () (apply-cont k2 env2))
      (<proc-12> ()
       (for-each-prim (car args) (cdr args) env2 handler k2))
      (<proc-13> ()
       (map-prim (car args) (cdr args) env2 handler k2))
      (<proc-14> () (apply-cont k2 (get-current-time)))
      (<proc-15> () (apply-cont k2 (dir args env2)))
      (<proc-16> () (apply-cont k2 (apply make-vector args)))
      (<proc-17> () (apply-cont k2 (apply append args)))
      (<proc-18> () (apply-cont k2 (apply reverse args)))
      (<proc-19> ()
       (call/cc-primitive (car args) env2 handler k2))
      (<proc-20> () (get-primitive args env2 handler k2))
      (<proc-21> () (import-primitive args env2 handler k2))
      (<proc-22> () (apply-cont k2 (apply set-cdr! args)))
      (<proc-23> () (apply-cont k2 (apply set-car! args)))
      (<proc-24> () (apply-cont k2 (apply range args)))
      (<proc-25> () (apply-cont k2 (apply memq args)))
      (<proc-26> () (apply-cont k2 (apply eq? args)))
      (<proc-27> () (apply-cont k2 (apply equal? args)))
      (<proc-28> () (apply-cont k2 (apply = args)))
      (<proc-29> () (apply-cont k2 (apply > args)))
      (<proc-30> () (apply-cont k2 (apply < args)))
      (<proc-31> ()
       (cond
         ((= (length args) 1)
          (if (= (car args) 0)
              (apply-handler handler "division by zero")
              (apply-cont k2 (apply / args))))
         ((>= (length args) 2)
          (if (= (cadr args) 0)
              (apply-handler handler "division by zero")
              (apply-cont k2 (apply / args))))
         (else (apply-handler handler "not enough args to /"))))
      (<proc-32> () (apply-cont k2 (apply * args)))
      (<proc-33> () (apply-cont k2 (apply - args)))
      (<proc-34> () (apply-cont k2 (apply + args)))
      (<proc-35> () (apply-cont k2 args))
      (<proc-36> () (apply-cont k2 (apply cdr args)))
      (<proc-37> () (apply-cont k2 (apply car args)))
      (<proc-38> () (apply-cont k2 (apply cons args)))
      (<proc-39> () (apply-cont k2 (apply null? args)))
      (<proc-40> ()
       (set! load-stack '())
       (load-file (car args) toplevel-env handler k2))
      (<proc-41> () (newline-prim) (apply-cont k2 '<void>))
      (<proc-42> ()
       (apply display-prim args)
       (apply-cont k2 '<void>))
      (<proc-43> ()
       (for-each pretty-print-prim args)
       (apply-cont k2 '<void>))
      (<proc-44> () (apply-cont k2 (apply sqrt args)))
      (<proc-45> ()
       (let ((proc (car args)) (proc-args (cadr args)))
         (apply-proc proc proc-args env2 handler k2)))
      (<proc-46> ()
       (read-datum
         (car args)
         handler
         (make-cont2 '<cont2-23> handler k2)))
      (<proc-47> () (parse (car args) handler k2))
      (<proc-48> ()
       (parse
         (car args)
         handler
         (make-cont '<cont-74> handler k2)))
      (<proc-49> ()
       (set! macro-env (make-macro-env))
       (set! toplevel-env (make-toplevel-env))
       (set! load-stack '())
       (halt* '(exiting the interpreter)))
      (<proc-50> (k) (apply-cont k (car args)))
      (<proc-51> (external-function-object)
       (apply-cont k2 (apply* external-function-object args)))
      (else (error 'apply-proc "bad procedure: ~a" proc)))))

;;----------------------------------------------------------------------
;; macro-transformer datatype

(define make-macro
  (lambda args (cons 'macro-transformer args)))

(define*
  apply-macro
  (lambda (macro datum k)
    (record-case (cdr macro)
      (<macro-1> ()
       (let ((name (caadr datum))
             (formals (cdadr datum))
             (bodies (cddr datum)))
         (apply-cont k `(define ,name (lambda ,formals ,@bodies)))))
      (<macro-2> ()
       (let ((exps (cdr datum)))
         (cond
           ((null? exps) (apply-cont k '#t))
           ((null? (cdr exps)) (apply-cont k (car exps)))
           (else
            (apply-cont k `(if ,(car exps) (and ,@(cdr exps)) #f))))))
      (<macro-3> ()
       (let ((exps (cdr datum)))
         (cond
           ((null? exps) (apply-cont k '#f))
           ((null? (cdr exps)) (apply-cont k (car exps)))
           (else
            (apply-cont
              k
              `(let ((bool ,(car exps))
                     (else-code (lambda () (or ,@(cdr exps)))))
                 (if bool bool (else-code))))))))
      (<macro-4> ()
       (let ((clauses (cdr datum)))
         (if (null? clauses)
             (error 'cond-transformer "bad concrete syntax: ~a" datum)
             (let ((first-clause (car clauses))
                   (other-clauses (cdr clauses)))
               (if (or (null? first-clause) (not (list? first-clause)))
                   (error 'cond-transformer
                     "bad concrete syntax: ~a"
                     datum)
                   (let ((test-exp (car first-clause))
                         (then-exps (cdr first-clause)))
                     (cond
                       ((eq? test-exp 'else)
                        (cond
                          ((null? then-exps)
                           (error 'cond-transformer
                             "bad concrete syntax: (~a)"
                             'else))
                          ((null? (cdr then-exps))
                           (apply-cont k (car then-exps)))
                          (else (apply-cont k `(begin ,@then-exps)))))
                       ((null? then-exps)
                        (if (null? other-clauses)
                            (apply-cont
                              k
                              `(let ((bool ,test-exp)) (if bool bool)))
                            (apply-cont
                              k
                              `(let ((bool ,test-exp)
                                     (else-code (lambda ()
                                                  (cond ,@other-clauses))))
                                 (if bool bool (else-code))))))
                       ((null? other-clauses)
                        (if (null? (cdr then-exps))
                            (apply-cont k `(if ,test-exp ,(car then-exps)))
                            (apply-cont
                              k
                              `(if ,test-exp (begin ,@then-exps)))))
                       ((null? (cdr then-exps))
                        (apply-cont
                          k
                          `(if ,test-exp
                               ,(car then-exps)
                               (cond ,@other-clauses))))
                       (else
                        (apply-cont
                          k
                          `(if ,test-exp
                               (begin ,@then-exps)
                               (cond ,@other-clauses)))))))))))
      (<macro-5> ()
       (if (symbol? (cadr datum))
           (let* ((name (cadr datum))
                  (bindings (caddr datum))
                  (vars (map car bindings))
                  (exps (map cadr bindings))
                  (bodies (cdddr datum)))
             (apply-cont
               k
               `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
           (let* ((bindings (cadr datum))
                  (vars (map car bindings))
                  (exps (map cadr bindings))
                  (bodies (cddr datum)))
             (apply-cont k `((lambda ,vars ,@bodies) ,@exps)))))
      (<macro-6> ()
       (let* ((decls (cadr datum))
              (vars (map car decls))
              (procs (map cadr decls))
              (bodies (cddr datum)))
         (create-letrec-assignments
           vars
           procs
           (make-cont2 '<cont2-12> bodies k))))
      (<macro-7> ()
       (let ((bindings (cadr datum)) (bodies (cddr datum)))
         (nest-let*-bindings bindings bodies k)))
      (<macro-8> ()
       (let ((exp (cadr datum)) (clauses (cddr datum)))
         (if (symbol? exp)
             (case-clauses->simple-cond-clauses
               exp
               clauses
               (make-cont '<cont-12> k))
             (case-clauses->cond-clauses
               'r
               clauses
               (make-cont2 '<cont2-14> exp k)))))
      (<macro-9> ()
       (let ((exp (cadr datum)) (clauses (cddr datum)))
         (if (symbol? exp)
             (record-case-clauses->cond-clauses
               exp
               clauses
               (make-cont2 '<cont2-16> k))
             (record-case-clauses->cond-clauses
               'r
               clauses
               (make-cont2 '<cont2-14> exp k)))))
      (else (error
             'apply-macro
             "bad macro-transformer: ~a"
             macro)))))

;;----------------------------------------------------------------------
;; main program

(define 1st (lambda (n) (string-ref chars-to-scan n)))

(define remaining (lambda (n) (+ 1 n)))

(define*
  scan-input
  (lambda (input handler k)
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 handler k)))

(define*
  scan-input-loop
  (lambda (chars handler k)
    (apply-action '(goto start-state) '() chars handler
      (make-cont2 '<cont2-1> handler k))))

(define scan-string
  (lambda (input)
    (set! read-line-count 1)
    (set! read-char-count 0)
    (scan-input input init-handler init-cont)))

(define scan-file
  (lambda (filename)
    (set! read-line-count 1)
    (set! read-char-count 0)
    (scan-input
      (read-content filename)
      init-handler
      init-cont)))

(define*
  apply-action
  (lambda (action buffer chars handler k)
    (record-case action
      (shift (next)
       (begin
         (set! read-char-count (+ read-char-count 1))
         (apply-action next (cons (1st chars) buffer)
           (remaining chars) handler k)))
      (replace (new-char next)
       (apply-action next (cons new-char buffer) (remaining chars)
         handler k))
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
       (convert-buffer-to-token
         token-type
         buffer
         handler
         (make-cont '<cont-3> chars k)))
      (else (error 'apply-action "invalid action: ~a" action)))))

(define*
  scan-error
  (lambda (chars handler)
    (let ((c (1st chars)))
      (if (char=? c #\nul)
          (apply-handler
            handler
            (format
              "unexpected end of input at line: ~a col: ~a"
              read-line-count
              read-char-count))
          (apply-handler
            handler
            (format
              "unexpected character ~a encountered at line: ~a col: ~a"
              c
              read-line-count
              read-char-count))))))

(define*
  convert-buffer-to-token
  (lambda (token-type buffer handler k)
    (let ((buffer (reverse buffer)))
      (case token-type
        (integer
         (apply-cont k (list 'integer (list->string buffer))))
        (decimal
         (apply-cont k (list 'decimal (list->string buffer))))
        (rational
         (apply-cont k (list 'rational (list->string buffer))))
        (identifier
         (apply-cont
           k
           (list 'identifier (string->symbol (list->string buffer)))))
        (boolean
         (apply-cont
           k
           (list
             'boolean
             (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
        (character (apply-cont k (list 'character (car buffer))))
        (named-character
         (let ((name (list->string buffer)))
           (cond
             ((string=? name "nul")
              (apply-cont k (list 'character #\nul)))
             ((string=? name "space")
              (apply-cont k (list 'character #\space)))
             ((string=? name "tab")
              (apply-cont k (list 'character #\tab)))
             ((string=? name "newline")
              (apply-cont k (list 'character #\newline)))
             ((string=? name "linefeed")
              (apply-cont k (list 'character #\newline)))
             ((string=? name "backspace")
              (apply-cont k (list 'character #\backspace)))
             ((string=? name "return")
              (apply-cont k (list 'character #\return)))
             ((string=? name "page")
              (apply-cont k (list 'character #\page)))
             (else
              (apply-handler
                handler
                (format
                  "invalid character name '~a' at line: ~a col: ~a"
                  name
                  read-line-count
                  read-char-count))))))
        (string (apply-cont k (list 'string (list->string buffer))))
        (else (apply-cont k (list token-type)))))))

(define token-type?
  (lambda (token class) (eq? (car token) class)))

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
  (lambda (c) (or (char=? c #\+) (char=? c #\-))))

(define char-boolean?
  (lambda (c)
    (or (char=? c #\t)
        (char=? c #\T)
        (char=? c #\f)
        (char=? c #\F))))

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
         ((char-alphabetic? c)
          '(shift (goto alphabetic-character-state)))
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
         ((or (char=? c #\e) (char=? c #\E))
          '(shift (goto suffix-state)))
         ((char-delimiter? c) '(emit integer))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (fractional-number-state
       (cond
         ((char-numeric? c) '(shift (goto fractional-number-state)))
         ((or (char=? c #\e) (char=? c #\E))
          '(shift (goto suffix-state)))
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
      (else (error 'apply-state "invalid state: ~a" state)))))

(define first (lambda (x) (car x)))

(define rest-of (lambda (x) (cdr x)))

(define read-string
  (lambda (input) (read-datum input init-handler init-cont2)))

(define*
  read-datum
  (lambda (input handler k)
    (set! read-char-count 0)
    (set! read-line-count 1)
    (scan-input input handler (make-cont '<cont-4> handler k))))

(define get-line-count (lambda (token) (rac (rdc token))))

(define get-char-count (lambda (token) (rac token)))

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

(define string->integer (lambda (str) (string->number str)))

(define string->decimal (lambda (str) (string->number str)))

(define string->rational
  (lambda (str) (string->number str)))

(define true? (lambda (v) (if v #t #f)))

(define*
  read-sexp
  (lambda (tokens handler k)
    (record-case (first tokens)
      (integer (str)
       (apply-cont2 k (string->integer str) (rest-of tokens)))
      (decimal (str)
       (apply-cont2 k (string->decimal str) (rest-of tokens)))
      (rational (str)
       (let ((num (string->rational str)))
         (if (true? num)
             (apply-cont2 k num (rest-of tokens))
             (apply-handler
               handler
               (format
                 "cannot represent ~a at line: ~a col: ~a"
                 str
                 (get-line-count (first tokens))
                 (get-char-count (first tokens)))))))
      (boolean (bool) (apply-cont2 k bool (rest-of tokens)))
      (character (char) (apply-cont2 k char (rest-of tokens)))
      (string (str) (apply-cont2 k str (rest-of tokens)))
      (identifier (id) (apply-cont2 k id (rest-of tokens)))
      (apostrophe () (read-abbreviation tokens 'quote handler k))
      (backquote ()
       (read-abbreviation tokens 'quasiquote handler k))
      (comma () (read-abbreviation tokens 'unquote handler k))
      (comma-at ()
       (read-abbreviation tokens 'unquote-splicing handler k))
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
       (read-vector
         (rest-of tokens)
         handler
         (make-cont2 '<cont2-4> k)))
      (else (read-error tokens handler)))))

(define*
  read-abbreviation
  (lambda (tokens keyword handler k)
    (read-sexp
      (rest-of tokens)
      handler
      (make-cont2 '<cont2-5> keyword k))))

(define*
  read-sexp-sequence
  (lambda (tokens expected-terminator handler k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator handler
         k))
      (dot ()
       (read-sexp
         (rest-of tokens)
         handler
         (make-cont2 '<cont2-8> expected-terminator handler k)))
      (else (read-sexp
             tokens
             handler
             (make-cont2 '<cont2-7> expected-terminator handler k))))))

(define*
  close-sexp-sequence
  (lambda (sexp tokens expected-terminator handler k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (cond
         ((token-type? (first tokens) expected-terminator)
          (apply-cont2 k sexp (rest-of tokens)))
         ((eq? expected-terminator 'rparen)
          (apply-handler
            handler
            (format
              "parenthesized list terminated by bracket at line: ~a col: ~a"
              (get-line-count (first tokens))
              (get-char-count (first tokens)))))
         ((eq? expected-terminator 'rbracket)
          (apply-handler
            handler
            (format
              "bracketed list terminated by parenthesis at line: ~a col: ~a"
              (get-line-count (first tokens))
              (get-char-count (first tokens)))))))
      (else (read-error tokens handler)))))

(define*
  read-vector
  (lambda (tokens handler k)
    (record-case (first tokens)
      (rparen () (apply-cont2 k '() (rest-of tokens)))
      (else (read-sexp
             tokens
             handler
             (make-cont2 '<cont2-9> handler k))))))

(define*
  read-error
  (lambda (tokens handler)
    (let ((token (first tokens)))
      (if (token-type? token 'end-marker)
          (apply-handler
            handler
            (format
              "unexpected end of input at line: ~a col: ~a"
              (get-line-count token)
              (get-char-count token)))
          (apply-handler
            handler
            (format
              "unexpected token ~a encountered at line: ~a col: ~a"
              (car token)
              (get-line-count token)
              (get-char-count token)))))))

(define read-file
  (lambda (filename)
    (scan-input
      (read-content filename)
      init-handler
      (make-cont '<cont-5>))))

(define*
  print-unparsed-sexps
  (lambda (tokens handler k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont k 'done)
        (read-sexp
          tokens
          handler
          (make-cont2 '<cont2-10> handler k)))))

(define read-next-sexp
  (lambda (tokens)
    (read-sexp tokens init-handler (make-cont2 '<cont2-11>))))

(define read-content
  (lambda (filename)
    (apply
      string
      (call-with-input-file
        filename
        (lambda (port)
          (let loop ((char (read-char port)))
            (if (eof-object? char)
                '()
                (cons char (loop (read-char port))))))))))

(define make-binding
  (lambda (variable value) (list variable "" value)))

(define binding-variable (lambda (binding) (car binding)))

(define binding-docstring (lambda (binding) (cadr binding)))

(define binding-value (lambda (binding) (caddr binding)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-car! (cdr binding) docstring)))

(define set-binding-value!
  (lambda (binding value) (set-car! (cddr binding) value)))

(define make-frame
  (lambda (variables values)
    (map make-binding variables values)))

(define first-binding (lambda (frame) (car frame)))

(define rest-of-bindings (lambda (frame) (cdr frame)))

(define empty-frame? (lambda (frame) (null? frame)))

(define search-frame
  (lambda (frame variable)
    (cond
      ((empty-frame? frame) #f)
      ((eq? (binding-variable (first-binding frame)) variable)
       (first-binding frame))
      (else (search-frame (rest-of-bindings frame) variable)))))

(define environment?
  (lambda (x) (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda () (cons 'environment '(()))))

(define make-initial-environment
  (lambda (vars vals)
    (cons 'environment (list (make-frame vars vals)))))

(define first-frame (lambda (env) (cadr env)))

(define frames (lambda (env) (cdr env)))

(define set-first-frame!
  (lambda (env new-frame) (set-car! (cdr env) new-frame)))

(define extend
  (lambda (env variables values)
    (cons
      'environment
      (cons (make-frame variables values) (cdr env)))))

(define search-env
  (lambda (env variable) (search-frames (cdr env) variable)))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
        #f
        (let ((binding (search-frame (car frames) variable)))
          (if binding
              binding
              (search-frames (cdr frames) variable))))))

(define*
  lookup-value
  (lambda (variable env handler k)
    (lookup-binding
      variable
      env
      handler
      (make-cont '<cont-6> k))))

(define*
  lookup-binding
  (lambda (variable env handler k)
    (let ((binding (search-env env variable)))
      (if binding
          (apply-cont k binding)
          (split-variable
            variable
            (make-cont '<cont-7> variable env handler k))))))

(define dlr-env-contains (lambda (variable) #t))

(define dlr-env-lookup (lambda (variable) (binding 42)))

(define*
  lookup-binding-in-first-frame
  (lambda (var env handler k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
            (apply-cont k binding)
            (let ((new-binding (make-binding var 'undefined)))
              (let ((new-frame (cons new-binding frame)))
                (set-first-frame! env new-frame)
                (apply-cont k new-binding))))))))

(define*
  lookup-variable-components
  (lambda (components path env handler k)
    (let ((var (car components)))
      (lookup-module-binding var env path handler
        (make-cont '<cont-8> components path var handler k)))))

(define*
  lookup-module-binding
  (lambda (var env path handler k)
    (let ((binding (search-env env var)))
      (cond
        (binding (apply-cont k binding))
        ((string=? path "")
         (apply-handler handler (format "unbound variable ~a" var)))
        (else
         (apply-handler
           handler
           (format "unbound variable ~a in module ~a" var path)))))))

(define*
  split-variable
  (lambda (variable k)
    (let ((strings (group
                     (string->list (symbol->string variable))
                     #\.)))
      (if (or (member "" strings) (= (length strings) 1))
          (apply-cont k #f)
          (apply-cont k (map string->symbol strings))))))

(define group
  (lambda (chars delimiter)
    (letrec ((position (lambda (chars)
                         (if (char=? (car chars) delimiter)
                             0
                             (+ 1 (position (cdr chars))))))
             (group (lambda (chars)
                      (cond
                        ((null? chars) '())
                        ((not (member delimiter chars))
                         (list (apply string chars)))
                        (else
                         (let ((n (position chars)))
                           (cons
                             (apply string (list-head chars n))
                             (group (cdr (list-tail chars n))))))))))
      (group chars))))

(define syntactic-sugar?
  (lambda (datum)
    (and (pair? datum)
         (symbol? (car datum))
         (true? (search-env macro-env (car datum))))))

(define make-pattern-macro
  (lambda (clauses) (cons 'pattern-macro clauses)))

(define macro-clauses (lambda (macro) (cdr macro)))

(define pattern-macro?
  (lambda (x) (and (pair? x) (eq? (car x) 'pattern-macro))))

(define*
  expand-once
  (lambda (datum handler k)
    (lookup-value
      (car datum)
      macro-env
      handler
      (make-cont '<cont-9> datum handler k))))

(define*
  process-macro-clauses
  (lambda (clauses datum handler k)
    (if (null? clauses)
        (apply-handler
          handler
          (format "no matching clause found for ~a" datum))
        (let ((left-pattern (caar clauses))
              (right-pattern (cadar clauses)))
          (unify-patterns
            left-pattern
            datum
            (make-cont '<cont-10> clauses datum right-pattern handler
              k))))))

(define*
  create-letrec-assignments
  (lambda (vars procs k2)
    (if (null? vars)
        (apply-cont2 k2 '() '())
        (create-letrec-assignments
          (cdr vars)
          (cdr procs)
          (make-cont2 '<cont2-13> procs vars k2)))))

(define*
  nest-let*-bindings
  (lambda (bindings bodies k)
    (if (or (null? bindings) (null? (cdr bindings)))
        (apply-cont k `(let ,bindings ,@bodies))
        (nest-let*-bindings
          (cdr bindings)
          bodies
          (make-cont '<cont-11> bindings k)))))

(define*
  case-clauses->simple-cond-clauses
  (lambda (var clauses k)
    (if (null? clauses)
        (apply-cont k '())
        (case-clauses->simple-cond-clauses
          var
          (cdr clauses)
          (make-cont '<cont-13> clauses var k)))))

(define*
  case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
        (apply-cont2 k2 '() '())
        (case-clauses->cond-clauses
          var
          (cdr clauses)
          (make-cont2 '<cont2-15> clauses var k2)))))

(define*
  record-case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
        (apply-cont2 k2 '() '())
        (record-case-clauses->cond-clauses
          var
          (cdr clauses)
          (make-cont2 '<cont2-17> clauses var k2)))))

(define make-macro-env
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case)
      (list and-transformer or-transformer cond-transformer
        let-transformer letrec-transformer let*-transformer
        case-transformer record-case-transformer))))

(define parse-string
  (lambda (string)
    (read-datum string init-handler (make-cont2 '<cont2-18>))))

(define*
  parse
  (lambda (datum handler k)
    (cond
      ((literal? datum) (apply-cont k (lit-exp datum)))
      ((quote? datum) (apply-cont k (lit-exp (cadr datum))))
      ((quasiquote? datum)
       (expand-quasiquote
         (cadr datum)
         handler
         (make-cont '<cont-30> handler k)))
      ((unquote? datum)
       (apply-handler handler (format "misplaced ~a" datum)))
      ((unquote-splicing? datum)
       (apply-handler handler (format "misplaced ~a" datum)))
      ((symbol? datum) (apply-cont k (var-exp datum)))
      ((syntactic-sugar? datum)
       (expand-once
         datum
         handler
         (make-cont '<cont-30> handler k)))
      ((if-then? datum)
       (parse
         (cadr datum)
         handler
         (make-cont '<cont-36> datum handler k)))
      ((if-else? datum)
       (parse
         (cadr datum)
         handler
         (make-cont '<cont-34> datum handler k)))
      ((assignment? datum)
       (parse
         (caddr datum)
         handler
         (make-cont '<cont-31> datum k)))
      ((define? datum)
       (if (mit-style? datum)
           (apply-macro
             mit-define-transformer
             datum
             (make-cont '<cont-30> handler k))
           (if (= (length datum) 3)
               (parse (caddr datum) handler (make-cont '<cont-29> datum k))
               (parse
                 (cadddr datum)
                 handler
                 (make-cont '<cont-28> datum handler k)))))
      ((define-syntax? datum)
       (apply-cont
         k
         (define-syntax-exp (cadr datum) (cddr datum))))
      ((begin? datum)
       (parse-all
         (cdr datum)
         handler
         (make-cont '<cont-26> datum handler k)))
      ((lambda? datum)
       (parse
         (cons 'begin (cddr datum))
         handler
         (make-cont '<cont-25> datum k)))
      ((try? datum)
       (cond
         ((= (length datum) 2) (parse (try-body datum) handler k))
         ((and (= (length datum) 3) (catch? (caddr datum)))
          (parse
            (try-body datum)
            handler
            (make-cont '<cont-24> datum handler k)))
         ((and (= (length datum) 3) (finally? (caddr datum)))
          (parse
            (try-body datum)
            handler
            (make-cont '<cont-22> datum handler k)))
         ((and (= (length datum) 4)
               (catch? (caddr datum))
               (finally? (cadddr datum)))
          (parse
            (try-body datum)
            handler
            (make-cont '<cont-20> datum handler k)))
         (else
          (apply-handler
            handler
            (format "bad try syntax: ~a" datum)))))
      ((raise? datum)
       (parse (cadr datum) handler (make-cont '<cont-17> k)))
      ((dict? datum)
       (parse-pairs (cdr datum) handler (make-cont '<cont-16> k)))
      ((application? datum)
       (parse
         (car datum)
         handler
         (make-cont '<cont-15> datum handler k)))
      (else
       (apply-handler
         handler
         (format "bad concrete syntax: ~a" datum))))))

(define*
  parse-pairs
  (lambda (pairs handler k)
    (if (null? pairs)
        (apply-cont k '())
        (parse
          (caar pairs)
          handler
          (make-cont '<cont-39> pairs handler k)))))

(define*
  parse-all
  (lambda (datum-list handler k)
    (if (null? datum-list)
        (apply-cont k '())
        (parse
          (car datum-list)
          handler
          (make-cont '<cont-41> datum-list handler k)))))

(define*
  expand-quasiquote
  (lambda (datum handler k)
    (cond
      ((vector? datum)
       (expand-quasiquote
         (vector->list datum)
         handler
         (make-cont '<cont-46> k)))
      ((not (pair? datum)) (apply-cont k `',datum))
      ((quasiquote? datum) (apply-cont k `',datum))
      ((unquote? datum) (apply-cont k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
           (apply-cont k (cadr (car datum)))
           (expand-quasiquote
             (cdr datum)
             handler
             (make-cont '<cont-45> datum k))))
      ((quasiquote-list? datum)
       (expand-quasiquote-list
         datum
         handler
         (make-cont '<cont-44> k)))
      (else
       (expand-quasiquote
         (car datum)
         handler
         (make-cont '<cont-43> datum handler k))))))

(define*
  expand-quasiquote-list
  (lambda (datum handler k)
    (if (null? datum)
        (apply-cont k '())
        (expand-quasiquote
          (car datum)
          handler
          (make-cont '<cont-48> datum handler k)))))

(define quasiquote-list?
  (lambda (datum)
    (or (null? datum)
        (and (pair? datum)
             (not (quasiquote? datum))
             (not (unquote? datum))
             (not (unquote-splicing? datum))
             (not (quasiquote? (car datum)))
             (not (unquote-splicing? (car datum)))
             (quasiquote-list? (cdr datum))))))

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((pair? (cdr formals))
       (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define mit-style?
  (lambda (datum) (not (symbol? (cadr datum)))))

(define literal?
  (lambda (datum)
    (or (number? datum)
        (boolean? datum)
        (char? datum)
        (string? datum)
        (vector? datum))))

(define anything? (lambda (datum) #t))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (and (list? datum)
           (op (length datum) len)
           (eq? (car datum) tag)))))

(define application?
  (lambda (datum)
    (and (list? datum)
         (not (null? datum))
         (not (reserved-keyword? (car datum))))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
         (memq
           x
           '(quote quasiquote lambda if set! define begin cond and or
              let let* letrec case record-case try catch finally raise
              dict)))))

(define try-body (lambda (x) (cadr x)))

(define catch-var (lambda (x) (cadr x)))

(define catch-exps (lambda (x) (cddr x)))

(define finally-exps (lambda (x) (cdr x)))

(define print-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (get-parsed-sexps filename))))

(define get-parsed-sexps
  (lambda (filename)
    (scan-input
      (read-content filename)
      init-handler
      (make-cont '<cont-49>))))

(define*
  parse-sexps
  (lambda (tokens handler k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont k '())
        (read-sexp
          tokens
          handler
          (make-cont2 '<cont2-19> handler k)))))

(define testall
  (lambda ()
    (read-datum
      "(load \"examples.ss\")"
      REP-handler
      (make-cont2 '<cont2-22>))))

(define start (lambda () (read-eval-print)))

(define pretty-print-prim
  (lambda (arg) (set! *need-newline* #f) (pretty-print arg)))

(define newline-prim
  (lambda () (set! *need-newline* #f) (newline)))

(define display-prim
  (lambda (arg)
    (let* ((s (format "~s" arg)) (len (string-length s)))
      (set! *need-newline*
        (true? (not (equal? (substring s (- len 1) len) "\n"))))
      (display s))))

(define read-line (lambda (prompt) (printf prompt) (read)))

(define*
  read-eval-print
  (lambda ()
    (let* ((input (read-line "==> "))
           (input-string (format "~s" input)))
      (read-datum
        input-string
        REP-handler
        (make-cont2 '<cont2-20>)))))

(define*
  m
  (lambda (exp env handler k)
    (cases expression exp (lit-exp (datum) (apply-cont k datum))
      (var-exp (id) (lookup-value id env handler k))
      (if-exp
        (test-exp then-exp else-exp)
        (m test-exp
           env
           handler
           (make-cont '<cont-70> else-exp then-exp env handler k)))
      (assign-exp
        (var rhs-exp)
        (m rhs-exp
           env
           handler
           (make-cont '<cont-69> var env handler k)))
      (define-exp
        (var rhs-exp)
        (if (= (length rhs-exp) 1)
            (m (car rhs-exp)
               env
               handler
               (make-cont '<cont-68> var env handler k))
            (m (cadr rhs-exp)
               env
               handler
               (make-cont '<cont-66> rhs-exp var env handler k))))
      (define-syntax-exp
        (keyword clauses)
        (lookup-binding-in-first-frame
          keyword
          macro-env
          handler
          (make-cont '<cont-63> clauses k)))
      (begin-exp (exps) (eval-sequence exps env handler k))
      (lambda-exp
        (formals body)
        (apply-cont k (closure formals body env)))
      (mu-lambda-exp
        (formals runt body)
        (apply-cont k (mu-closure formals runt body env)))
      (try-catch-exp
        (body cvar cexps)
        (let ((new-handler (try-catch-handler cvar cexps env handler
                             k)))
          (m body env new-handler k)))
      (try-finally-exp
        (body fexps)
        (let ((new-handler (try-finally-handler fexps env handler)))
          (m body
             env
             new-handler
             (make-cont '<cont-62> fexps env handler k))))
      (try-catch-finally-exp
        (body cvar cexps fexps)
        (let ((new-handler (try-catch-finally-handler cvar cexps
                             fexps env handler k)))
          (m body
             env
             new-handler
             (make-cont '<cont-62> fexps env handler k))))
      (raise-exp
        (exp)
        (m exp env handler (make-cont '<cont-60> handler)))
      (dict-exp (pairs) (apply-cont k (list 'dict pairs)))
      (app-exp
        (operator operands)
        (m* operands
            env
            handler
            (make-cont '<cont-59> operator env handler k)))
      (else (error 'm "bad abstract syntax: ~a" exp)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (make-handler '<handler-3> cexps cvar env handler k)))

(define try-finally-handler
  (lambda (fexps env handler)
    (make-handler '<handler-4> fexps env handler)))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (make-handler '<handler-5> cexps cvar fexps env handler k)))

(define closure
  (lambda (formals body env)
    (make-proc '<proc-1> formals body env)))

(define mu-closure
  (lambda (formals runt body env)
    (make-proc '<proc-2> formals runt body env)))

(define*
  m*
  (lambda (exps env handler k)
    (if (null? exps)
        (apply-cont k '())
        (m (car exps)
           env
           handler
           (make-cont '<cont-72> exps env handler k)))))

(define*
  eval-sequence
  (lambda (exps env handler k)
    (m (car exps)
       env
       handler
       (make-cont '<cont-73> exps env handler k))))

(define make-initial-env-extended (lambda (env) env))

(define make-toplevel-env
  (lambda ()
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
         (make-proc '<proc-4>) (make-proc '<proc-3>))))))

(define*
  map-prim
  (lambda (proc args env handler k)
    (let ((len (length args)) (list-args (listify args)))
      (cond
        ((= len 1) (map1 proc (car list-args) env handler k))
        ((= len 2)
         (map2 proc (car list-args) (cadr list-args) env handler k))
        (else (mapN proc list-args env handler k))))))

(define listify
  (lambda (arg-list)
    (cond
      ((null? arg-list) '())
      ((list? (car arg-list))
       (cons (car arg-list) (listify (cdr arg-list))))
      ((vector? (car arg-list))
       (cons
         (my-vector->list (car arg-list))
         (listify (cdr arg-list))))
      ((string? (car arg-list))
       (cons
         (string->list (car arg-list))
         (listify (cdr arg-list))))
      (else
       (error 'map
         "cannot use object type '~a' in map"
         (get_type (car arg-list)))))))

(define*
  map1
  (lambda (proc list1 env handler k)
    (if (null? list1)
        (apply-cont k '())
        (if (dlr-exp? proc)
            (map1 proc (cdr list1) env handler
              (make-cont '<cont-76> list1 proc k))
            (apply-proc proc (list (car list1)) env handler
              (make-cont '<cont-75> list1 proc env handler k))))))

(define*
  map2
  (lambda (proc list1 list2 env handler k)
    (if (null? list1)
        (apply-cont k '())
        (if (dlr-exp? proc)
            (map2 proc (cdr list1) (cdr list2) env handler
              (make-cont '<cont-78> list1 list2 proc k))
            (apply-proc proc (list (car list1) (car list2)) env handler
              (make-cont '<cont-77> list1 list2 proc env handler k))))))

(define*
  mapN
  (lambda (proc lists env handler k)
    (if (null? (car lists))
        (apply-cont k '())
        (if (dlr-exp? proc)
            (mapN proc (map cdr lists) env handler
              (make-cont '<cont-80> lists proc k))
            (apply-proc proc (map car lists) env handler
              (make-cont '<cont-79> lists proc env handler k))))))

(define*
  for-each-prim
  (lambda (proc lists env handler k)
    (let ((arg-list (listify lists)))
      (if (null? (car arg-list))
          (apply-cont k '<void>)
          (if (dlr-exp? proc)
              (begin
                (dlr-apply proc (map car arg-list))
                (for-each-prim proc (map cdr arg-list) env handler k))
              (apply-proc proc (map car arg-list) env handler
                (make-cont '<cont-81> arg-list proc env handler k)))))))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
         (inexact (/ (time-nanosecond now) 1000000000))))))

(define*
  get-primitive
  (lambda (args env handler k)
    (let ((sym (car args)))
      (lookup-value
        sym
        env
        handler
        (make-cont '<cont-82> args sym handler k)))))

(define*
  import-primitive
  (lambda (args env handler k)
    (let ((filename (car args)))
      (if (null? (cdr args))
          (load-file filename env handler k)
          (let ((module-name (cadr args)))
            (lookup-binding-in-first-frame
              module-name
              env
              handler
              (make-cont '<cont-83> filename env handler k)))))))

(define*
  call/cc-primitive
  (lambda (proc env handler k)
    (let ((fake-k (make-proc '<proc-50> k)))
      (if (dlr-exp? proc)
          (apply-cont k (dlr-apply proc (list fake-k)))
          (apply-proc proc (list fake-k) env handler k)))))

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

(define dir
  (lambda (args env)
    (sort
      symbol<?
      (if (null? args)
          (flatten
            (append
              (map get-variables-from-frame (frames macro-env))
              (map get-variables-from-frame (frames env))))
          (get-variables-from-frame (car (frames (car args))))))))

(define get-variables-from-frame
  (lambda (frame) (map binding-variable frame)))

(define symbol<?
  (lambda (a b)
    (let ((a_string (symbol->string a))
          (b_string (symbol->string b)))
      (string<? a_string b_string))))

(define*
  load-file
  (lambda (filename env handler k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (apply-cont k '<void>))
      ((not (string? filename))
       (apply-handler
         handler
         (format "filename is not a string: ~a" filename)))
      ((not (file-exists? filename))
       (apply-handler
         handler
         (format "file does not exist: ~a" filename)))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input
         (read-content filename)
         handler
         (make-cont '<cont-85> env handler k))))))

(define*
  load-loop
  (lambda (tokens env handler k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont k '<void>)
        (read-sexp
          tokens
          handler
          (make-cont2 '<cont2-24> env handler k)))))

(define*
  load-files
  (lambda (filenames env handler k)
    (if (null? filenames)
        (apply-cont k 'ok)
        (load-file
          (car filenames)
          env
          handler
          (make-cont '<cont-88> filenames env handler k)))))

(define*
  help-prim
  (lambda (var env handler k)
    (lookup-binding var env handler (make-cont '<cont-89> k))))

(define range
  (lambda args
    (letrec ((range (lambda (n end step acc)
                      (if (>= n end)
                          (reverse acc)
                          (range (+ n step) end step (cons n acc))))))
      (cond
        ((null? (cdr args)) (range 0 (car args) 1 '()))
        ((null? (cddr args)) (range (car args) (cadr args) 1 '()))
        (else (range (car args) (cadr args) (caddr args) '()))))))

(define make-external-proc
  (lambda (external-function-object)
    (make-proc '<proc-51> external-function-object)))

(define Main
  (lambda (args)
    (printf "Pyjama Scheme (0.1)\n")
    (printf "(c) 2009, IPRE\n")
    (load-files (list args) toplevel-env REP-handler REP-k)
    (trampoline)))

(define execute
  (lambda (input-string)
    (read-datum
      input-string
      init-handler
      (make-cont2 '<cont2-25>))
    (trampoline)))

(define pattern?
  (lambda (x)
    (or (null? x)
        (number? x)
        (boolean? x)
        (symbol? x)
        (and (pair? x) (pattern? (car x)) (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
         (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x)) (not (pair? x)))))

(define*
  occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (apply-cont k #f))
      ((pattern-variable? pattern)
       (apply-cont k (equal? var pattern)))
      (else
       (occurs?
         var
         (car pattern)
         (make-cont '<cont-91> pattern var k))))))

(define*
  unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
           (apply-cont k (make-sub 'unit p1 p2))
           (occurs? p1 p2 (make-cont '<cont-92> p1 p2 k))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2))
       (apply-cont k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (apply-cont k #f)))))

(define*
  unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns
      (car pair1)
      (car pair2)
      (make-cont '<cont-96> pair1 pair2 k))))

(define*
  instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (apply-cont k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate
         (car pattern)
         s
         (make-cont '<cont-97> pattern s k)))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

(define make-sub (lambda args (cons 'substitution args)))

(define*
  apply-sub
  (lambda (s var k)
    (record-case (cdr s)
      (empty () (apply-cont k var))
      (unit (new-var new-pattern)
       (if (equal? var new-var)
           (apply-cont k new-pattern)
           (apply-cont k var)))
      (composite (s1 s2)
       (apply-sub s1 var (make-cont '<cont-98> s2 k)))
      (else (error 'apply-sub "bad substitution: ~a" s)))))

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

