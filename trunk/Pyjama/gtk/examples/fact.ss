(define factorial
    (lambda (n)
        (if (eq? n 1)
            1
            (* n (factorial (- n 1))))))
