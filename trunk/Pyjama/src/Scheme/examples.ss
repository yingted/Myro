(define odd? 'undefined)
(define even? 'undefined)

(letrec
    ((odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
     (even (lambda (n) (if (= n 0) #t (odd (- n 1))))))
  (set! odd? odd)
  (set! even? even))

;;---------------------------------------------------------------------
;; collect is like list comprehension in Python

(define-syntax collect
  [(collect ?exp for ?var in ?list)
   (filter-map (lambda (?var) ?exp) (lambda (?var) #t) ?list)]
  [(collect ?exp for ?var in ?list if ?condition)
   (filter-map (lambda (?var) ?exp) (lambda (?var) ?condition) ?list)])

(define filter-map
  (lambda (f pred? values)
    (if (null? values)
      '()
      (if (pred? (car values))
	  (cons (f (car values)) (filter-map f pred? (cdr values)))
	  (filter-map f pred? (cdr values))))))

;;---------------------------------------------------------------------
;; for loops

(define-syntax for
  [(for ?exp times do . ?bodies)
   (for-repeat ?exp (lambda () . ?bodies))]
  [(for ?var in ?exp do . ?bodies)
   (for-iterate1 ?exp (lambda (?var) . ?bodies))]
  [(for ?var at (?i) in ?exp do . ?bodies)
   (for-iterate2 0 ?exp (lambda (?var ?i) . ?bodies))]
  [(for ?var at (?i ?j . ?rest) in ?exp do . ?bodies)
   (for ?var at (?i) in ?exp do
     (for ?var at (?j . ?rest) in ?var do . ?bodies))])

(define for-repeat
  (lambda (n f)
    (if (< n 1)
      'done
      (begin
	(f)
	(for-repeat (- n 1) f)))))

(define for-iterate1
  (lambda (values f)
    (if (null? values)
      'done
      (begin
	(f (car values))
	(for-iterate1 (cdr values) f)))))

(define for-iterate2
  (lambda (i values f)
    (if (null? values)
      'done
      (begin
	(f (car values) i)
	(for-iterate2 (+ i 1) (cdr values) f)))))

(define matrix2D
  '((10 20)
    (30 40)
    (50 60)
    (70 80)))

(define matrix3D
  '(((10 20 30) (40 50 60))
    ((70 80 90) (100 110 120))
    ((130 140 150) (160 170 180))
    ((190 200 210) (220 230 240))))

;;---------------------------------------------------------------------
;; streams

(define-syntax scons
  [(scons ?x ?y) (cons ?x (lambda () ?y))])

(define scar car)

(define scdr
  (lambda (s)
    (let ((result ((cdr s))))
      (set-cdr! s (lambda () result))
      result)))

(define first
  (lambda (n s)
    (if (= n 0)
      '()
      (cons (scar s) (first (- n 1) (scdr s))))))

(define nth
  (lambda (n s)
    (if (= n 0)
      (scar s)
      (nth (- n 1) (scdr s)))))

(define smap
  (lambda (f s)
    (scons (f (scar s)) (smap f (scdr s)))))

(define ones (scons 1 ones))

(define nats (scons 0 (combine nats + ones)))

(define combine
  (lambda (s1 op s2)
    (scons (op (scar s1) (scar s2)) (combine (scdr s1) op (scdr s2)))))

(define fibs (scons 1 (scons 1 (combine fibs + (scdr fibs)))))

(define facts (scons 1 (combine facts * (scdr nats))))

(define ! (lambda (n) (nth n facts)))

;;---------------------------------------------------------------------
;; example interaction:

;; > (load "interpreter-cps.ss")
;; Loaded EOPL init file
;; > (start)
;; ==> "(load \"examples.ss\")"
;; ok
;; ==> "(odd? 43)"
;; #t
;; ==> quit
;; (exiting the interpreter)

;; other examples:

;; ==> "(list (odd? 42) (even? 42) (odd? 43) (even? 43))"

;; ==> "(collect (* n n) for n in (range 10))"
;; ==> "(collect (* n n) for n in (range 5 20 3))"
;; ==> "(collect (* n n) for n in (range 10) if (> n 5))"

;; ==> "(for 5 times do (print 'hello))"
;; ==> "(for sym in '(a b c d) do (print sym) (newline))"
;; ==> "(for n in (range 10 20 2) do (print n))"

;; ==> "(for n at (i j) in matrix2D do (print (list n 'coords: i j)))"
;; ==> "(for n at (i j k) in matrix3D do (print (list n 'coords: i j k)))"

;; ==> "(! 5)"
;; ==> "(nth 10 facts)"
;; ==> "(nth 20 fibs)"
;; ==> "(first 30 fibs)"

(define test-all
  (lambda ()
    (print (list (odd? 42) (even? 42) (odd? 43) (even? 43)))
    (print (collect (* n n) for n in (range 10)))
    (print (collect (* n n) for n in (range 5 20 3)))
    (print (collect (* n n) for n in (range 10) if (> n 5)))
    (print (for 5 times do (print 'hello)))
    (print (for sym in '(a b c d) do (print sym) (newline)))
    (print (for n in (range 10 20 2) do (print n)))
    (print (for n at (i j) in matrix2D do (print (list n 'coords: i j))))
    (print (for n at (i j k) in matrix3D do (print (list n 'coords: i j k))))
    (print (! 5))
    (print (nth 10 facts))
    (print (nth 20 fibs))
    (print (first 30 fibs))))

;; try/catch

