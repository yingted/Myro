(define *ignore-functions*
  '(read-content anything? last
    record-case-transformer let*-transformer 
    letrec-transformer let-transformer cond-transformer
    or-transformer range group search-env make-empty-environment
    search-frame apply-proc make-proc make-handler make-cont2
    raise-exp try-catch-finally-exp try-finally-exp try-catch-exp
    app-exp mu-lambda-exp lambda-exp begin-exp define-syntax-exp
    define-exp assign-exp if-exp var-exp lit-exp
    data-structure-procedure? module? syntactic-sugar?
true?
lit-exp
var-exp
if-exp
assign-exp
define-exp
define-syntax-exp
begin-exp
lambda-exp
mu-lambda-exp
app-exp
try-catch-exp
try-finally-exp
try-catch-finally-exp
raise-exp
make-cont
make-cont2
make-handler
make-proc
make-binding
binding-variable
binding-value
set-binding-value!
make-frame
first-binding
rest-of-bindings
empty-frame?
search-frame
make-empty-environment
make-initial-environment
first-frame
rest-of-frames
set-first-frame!
extend
search-env
group
start
read-eval-print
data-structure-procedure?
safe-print
try-catch-handler
try-finally-handler
try-catch-finally-handler
closure
mu-closure
make-toplevel-env
module?
get-variables
range
syntactic-sugar?
mit-define-transformer
and-transformer
or-transformer
cond-transformer
let-transformer
letrec-transformer
let*-transformer
record-case-transformer
make-macro-env
parse-string
head
last
mit-style?
literal?
anything?
tagged-list
application?
reserved-keyword?
print-parsed-sexps
get-parsed-sexps
1st
remaining
scan-string
scan-file
token-type?
char-delimiter?
char-initial?
char-special-subsequent?
char-subsequent?
char-sign?
char-boolean?
apply-state
read-string
read-file
read-next-sexp
read-content
pattern?
pattern-variable?
constant?
make-sub
REP-k
REP-handler
load-stack
toplevel-env
macro-env
macro-env
quote?
quasiquote?
unquote?
unquote-splicing?
if-then?
if-else?
assignment?
define?
define-syntax?
begin?
lambda?
raise?
try?
try-body
catch?
catch-var
catch-exps
finally?
finally-exps
chars-to-scan
init-cont
init-cont2
init-handler
first
rest-of
    ))

(define *function-signatures*
  '(
    ))


