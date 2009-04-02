(module char-utils
  (char-titlecase char-cased?)
  (import scheme)

(define (char-cased? c)
  (not (char=? (char-downcase c)
			   (char-upcase c))))

(define char-titlecase char-upcase)

)
