;;; -*- Mode: scheme -*-

;;;; Scribbler Robot + Fluke API

;;; Copyright (c) 2009, Aaron W. Hsu
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(eval-when (compile) 
  (generate-inspector-information #f)
  (optimize-level 3))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Load the Shared Objects
(define-syntax load-objects
  (lambda (x)
    (syntax-case x ()
      [(k)
       (let* ([mt (symbol->string (machine-type))]
              [file (string-append "sys" (string (directory-separator))
                                   mt "-objs.ss")])
         #`(#,(datum->syntax #'k 'include) #,file))])))

(load-objects)

(module 
  (beep
   latency
   initialize-scribbler
   disconnect-scribbler
   get-info
   set-speaker
   set-speaker2
   get-battery
   senses
   get-obstacle
   get-bright
   stop
   forward
   backward
   turn-left
   turn-right
   translate
   rotate
   motors
   move
   get-name
   get-password
   get-volume
   get-data
   wall?
   set-led!
   set-name!
   set-volume!
   set-data!
   set-ir-power!
   get-forwardness
   set-forwardness!)
  (import scheme)

  (include "lib/syn-param.ss")
  (include "lib/char-utils.ss")
  (include "lib/let-opt.ss")
  (include "lib/check-arg.ss")
  (include "lib/srfi-8.ss")
  (include "lib/foof-loop.ss")
  (include "lib/nested-foof-loop.ss")
  (include "lib/srfi-14.ss")
  (include "lib/srfi-13.ss")
  (include "lib/arcfide-misc.ss")
  
  (import (rename arcfide-misc (simple-string-split string-split)))
  (import foof-loop)
  (import nested-foof-loop)
  (import srfi-13)

(define scribbler-debug? 
  (make-parameter #f 
    (lambda (x) 
      (if (boolean? x) 
          x 
          (error 'scribbler-debug? "~a is not a boolean." x)))))

(define-record scribbler (in out disconnect) ((volume #t)))

(define latency
  (make-parameter 0.03 
    (lambda (x) 
      (if (number? x) x (error 'latency "~a is not a number" x)))))

(define initialize-scribbler
  (lambda (addr)
    (let-values ([(iport oport disconnect) (connect-scribbler addr)])
      (let ([bot (make-scribbler iport oport disconnect)])
        (printf "Hello! My name is ~a.~n" (get-name bot))
        (for-each (lambda (e)
                    (printf "~a: ~a~n" (car e) (cdr e)))
          (get-info bot))
        (set-volume! bot #t)
        (conf-gray-window bot 0 2 0 128 191 1 1)
        (conf-gray-window bot 1 64 0 190 191 1 1)
        (conf-gray-window bot 2 128 0 254 191 1 1)
        bot))))

(define disconnect-scribbler
  (lambda (bot)
    (close-port (scribbler-in bot))
    (close-port (scribbler-out bot))
    ((scribbler-disconnect bot))))

(define get-info 
  (lambda (bot)
    (let ([out (scribbler-out bot)]
          [in (scribbler-in bot)])
      (clear-input-port in)
      (scribbler-send out code/get-info) ; Stupid thing has to be run twice.
      (read-line in)
      (scribbler-send out code/get-info)
      (when (char-ci=? #\p (peek-char in))
        (read-char in))
      (map (lambda (e) (cons (string->symbol (car e)) (cadr e)))
        (map (lambda (s) (map string-trim (simple-string-split #\: s)))
          (simple-string-split #\, (read-line in)))))))

(define beep
  (case-lambda
    [(bot duration frequency)
     (set-speaker bot frequency (exact (* 1000 duration)))
     (sleep (latency))
     (sleep duration)
     (void)]
    [(bot duration frequency frequency2)
     (set-speaker2 bot frequency frequency2 (exact (* 1000 duration)))
     (sleep (latency))
     (sleep duration)
     (void)]))

(define set-speaker
  (lambda (bot frequency duration)
    (scribbler-send (scribbler-out bot) code/set-speaker
      (fxsrl duration 8)
      (modulo duration 256)
      (fxsrl frequency 8)
      (modulo frequency 256))))

(define set-speaker2
  (lambda (bot frequency frequency2 duration)
    (scribbler-send (scribbler-out bot) code/set-speaker2
      (fxsrl duration 8)
      (modulo duration 256)
      (fxsrl frequency 8)
      (modulo frequency 256)
      (fxsrl frequency2 8)
      (modulo frequency2 256))))

(define get-battery
  (lambda (bot)
    (let ([out (scribbler-out bot)]
          [in (scribbler-in bot)])
      (display (integer->char code/get-battery) out)
      (flush-output-port out)
      (/ (read-bytes->number in 2) 20.9813))))

(define upgrade-scribbler
  (lambda (bot file)
    (let ([bytes (read-bytes file)])
      (iterate! (for byte i (in-vector bytes))
        (set-scribbler-memory bot i byte))
      ;; Debug
      (sleep 5)
      (iterate! ([for byte offset (in-vector bytes)])
        (let ([actual (get-scribbler-memory bot offset)])
          (unless (char=? byte actual)
            (error 'upgrade-scribbler
              "~s does not match intended value ~s ~n" actual byte))))
      (set-scribbler-start-program bot (vector-length bytes))
      (sleep 10)
      (printf "Please restart your Scribbler.~n"))))

(define upgrade-fluke
  (lambda (bot filename)
    (unless (file-exists? filename)
      (error 'upgrade-fluke
        "Cannot find ~s" filename))
    (let* ([ih-buffer (load-intelhex filename)]
           [ih-vector (intelhex->vector ih-buffer)]
           [op (scribbler-out bot)]
           [ip (scribbler-in bot)])
      (flush-output-port op)
      (format op "~a~a~a"
        (integer->char code/upgrade-fluke)
        (integer->char #x01)
        (integer->char #x23))
      (fluke-store-in-eeprom ip op ih-vector)
      (sleep 2))))

(define set-scribbler-memory
  (lambda (bot i byte)
    (assert-type byte char? "character" 'set-scribbler-memory)
    (let ([op (scribbler-out bot)])
      (display (integer->char code/set-scribbler-program) op)
      (write-number/bytes i 2 op) 
      (display byte op)
      (flush-output-port op))))

(define get-scribbler-memory
  (lambda (bot offset)
    (let ([ip (scribbler-in bot)]
          [op (scribbler-out bot)])
      (display (integer->char code/get-scribbler-program) op)
      (write-number/bytes offset 2 op) ;; Broken Firmware = 1 byte addr
      (flush-output-port op)
      (read-char ip))))

(define set-scribbler-start-program
  (lambda (bot size)
    (let ([op (scribbler-out bot)])
      (display (integer->char code/set-start-program) op)
      (display (integer->char 1) op)
      (display (integer->char #x23) op)
      (write-number/bytes size 2 op)
      (flush-output-port op))))

(define senses
  (lambda (bot)
    (let ([out (scribbler-out bot)]
          [in (scribbler-in bot)])
      (scribbler-send out code/get-all)
      (let ([vals (vector-map char->integer (scribbler-read in 11))])
        `((light . ,(light-value vals))
          (ir . ,(ir-value vals))
          (line . ,(line-value vals))
          (stall . ,(stall-value vals))
          (obstacle . ,(get-obstacle bot 'left 'center 'right))
          (bright . ,(get-bright bot 'left 'center 'right))
          (blob . unsupported)
          (battery . ,(get-battery bot)))))))

(define get-obstacle
  (case-lambda
    [(bot direction)
     (%get-obstacle (scribbler-in bot) (scribbler-out bot) direction)]
    [(bot . dirs)
     (let ([out (scribbler-out bot)]
           [in (scribbler-in bot)])
       (map (lambda (direction) (%get-obstacle in out direction)) dirs))]))

(define get-bright
  (case-lambda
    [(bot direction)
     (%get-bright bot direction)]
    [(bot . dirs)
     (map (lambda (dir) (%get-bright bot dir)) dirs)]))

(define conf-window
  (lambda (bot . settings)
    (unless (and (= 7 (length settings)) (andmap integer? settings))
      (error 'conf-window "Invalid settings."))
    (let ([out (scribbler-out bot)])
      (for-each (lambda (i) (display (integer->char i) out)) 
        (cons code/set-window settings))
      (flush-output-port out))))

(define conf-gray-window
  (lambda (bot window lx ly ux uy xstep ystep)
    (let ([lx (if (even? lx) (1+ lx) lx)]
          [xstep (if (even? xstep) xstep (1+ xstep))])
      (conf-window bot window lx ly ux uy xstep ystep))))

(define get-blob
  (lambda (bot)
    (let ([out (scribbler-out bot)]
          [in (scribbler-in bot)])
      (display (integer->char code/get-blob) out)
      (flush-output-port out)
      (let* ([numpixs (char->integer (read-char in))]
             [xloc (char->integer (read-char in))]
             [yloc (char->integer (read-char in))])
        (list numpixs xloc yloc)))))

(define stop
  (lambda (bot)
    (let ([out (scribbler-out bot)]
          [in (scribbler-in bot)])
      (scribbler-send out code/set-motors-off)
      (flush-output-port out)
      (iterate! (for i (up-from 0 (to 20))) (read-char in)))))

(define forward
  (case-lambda 
    [(bot) (%translate bot 1 0)]
    [(bot speed) (%translate bot speed 0)]
    [(bot speed interval) (%translate bot speed interval)]))

(define backward
  (case-lambda
    [(bot) (%translate bot -1 0)]
    [(bot speed) (%translate bot (- speed) 0)]
    [(bot speed interval) (%translate bot (- speed) interval)]))

(define turn-left
  (case-lambda
    [(bot) (%rotate bot 1 0)]
    [(bot speed) (%rotate bot speed 0)]
    [(bot speed interval) (%rotate bot speed interval)]))

(define turn-right
  (case-lambda
    [(bot) (%rotate bot -1 0)]
    [(bot speed) (%rotate bot (- speed) 0)]
    [(bot speed interval) (%rotate bot (- speed) interval)]))

(define translate (lambda (bot amount) (%translate bot amount 0)))
(define rotate (lambda (bot amount) (%rotate bot amount 0)))

(define motors
  (lambda (bot left right)
    (let ([trans (/ (+ right left) 2)]
          [rotate (/ (- right left) 2)])
      (move bot trans rotate))))

(define move
  (lambda (bot translate rotate)
    (let ([left (min 1 (max -1 (- translate rotate)))]
          [right (min 1 (max -1 (+ translate rotate)))]
          [in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (let ([left-power (* 100 (1+ left))]
            [right-power (* 100 (1+ right))])
        (scribbler-send out code/set-motors right-power left-power)
        (flush-output-port out)
        (iterate! (for i (up-from 0 (to 20))) (read-char in))))))

(define get-name
  (lambda (bot)
    (let ([in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (clear-input-port in)
      (flush-output-port out)
      (let* ([clst1 (begin (scribbler-send out code/get-name1)
                      (flush-output-port out)
                      (vector->list (scribbler-read in 8)))]
             [clst2 (begin (scribbler-send out code/get-name2)
                      (flush-output-port out)
                      (vector->list (scribbler-read in 8)))])
        (string-trim-both (list->string (append clst1 clst2)))))))

(define get-password
  (lambda (bot)
    (let ([in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (scribbler-send out code/get-password1)
      (flush-output-port out)
      (scribbler-send out code/get-password2)
      (flush-output-port out)
      (let* ([clst1 (vector->list (scribbler-read in 8))]
             [clst2 (vector->list (scribbler-read in 8))])
        (string-trim-both (list->string (append clst1 clst2)))))))

(define get-volume
  (lambda (bot)
    (scribbler-volume bot)))

(define get-data
  (lambda (bot . pos)
    (scribbler-send (scribbler-out bot) code/get-data)
    (let ([res (scribbler-read (scribbler-in bot) 8)])
      (if (null? pos)
          res
          (collect-vector-of-length (length pos) 
              (for i (in-list pos))
            (vector-ref res i))))))

(define wall?
  (lambda (bot)
    (< 4500 (get-obstacle bot 'center))))

(define set-led!
  (lambda (bot led-pos on)
    (let ([in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (case led-pos
        [(left)
         (scribbler-send out 
           (if on code/set-led-left-on code/set-led-left-off))
         (scribbler-read in 11)]
        [(center)
         (scribbler-send out 
           (if on code/set-led-center-on code/set-led-center-off))
         (scribbler-read in 11)]
        [(right)
         (scribbler-send out
           (if on code/set-led-right-on code/set-led-right-off))
         (scribbler-read in 11)]
        [(all)
         (scribbler-send out
           (if on code/set-led-all-on code/set-led-all-off))
         (scribbler-read in 11)]
        [(front)
         (display 
           (integer->char 
             (if on code/set-dongle-led-on code/set-dongle-led-off))
           out)
         (flush-output-port out)]
        [(back)
         (let ([val (if (< 1 on) 1 (if (> 0 on) 0 on))])
           (display (integer->char code/set-dimmer-led) out)
           (display 
	     (integer->char (exact (round (+ 170 (* val (- 255 170)))))) 
	     out)
           (flush-output-port out))]
        [else (error 'set-led "Invalid led ~a" led-pos)])
      (void))))

(define set-name!
  (lambda (bot name)
    (let ([pname (string-pad name 16)]
          [in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (let ([part1 (map char->integer (string->list (substring pname 0 8)))]
            [part2 (map char->integer (string->list (substring pname 8 16)))])
        (apply scribbler-send `(,out ,code/set-name1 . ,part1))
        (flush-output-port out)
        (scribbler-read in 11)
        (apply scribbler-send `(,out ,code/set-name2 . ,part2))
        (flush-output-port out)
        (scribbler-read in 11)
        (void)))))

(define set-volume!
  (lambda (bot on?)
    (let ([in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (set-scribbler-volume! bot on?)
      (if on?
          (scribbler-send out code/set-loud)
          (scribbler-send out code/set-quiet))
      (scribbler-read in 11)
      (void))))

(define set-data!
  (lambda (bot address value)
    (let ([cur (get-data bot)])
      (vector-set! cur address value)
      (apply scribbler-send 
        `(,(scribbler-out bot) ,code/set-data 
          . ,(map char->integer (vector->list cur))))
      (flush-output-port (scribbler-out bot))
      (scribbler-read (scribbler-in bot) 11)
      (void))))

(define set-ir-power!
  (lambda (bot power)
    (let ([out (scribbler-out bot)])
      (display (integer->char code/set-dongle-ir) out)
      (display (integer->char power) out)
      (flush-output-port out))))

(define get-forwardness
  (lambda (bot)
    (let ([in (scribbler-in bot)]
          [out (scribbler-out bot)])
      (case (read-serial-memory in out 0 0)
        [(#\ß) 'scribbler]
        [else 'fluke]))))

(define set-forwardness!
  (lambda (bot direction)
    (let ([dir-val (case direction 
                     [(fluke) 1] 
                     [(scribbler) 0]
                     [else (error 'set-forwardness! 
                             "Bad direction ~a" direction)])]
          [out (scribbler-out bot)])
      (display (integer->char code/set-forwardness) out)
      (display (integer->char dir-val) out)
      (flush-output-port out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Low Level Communication

(define scribbler-send
  (lambda (out . data) 
    (when (< 9 (length data))
      (error 'scribbler-send "~s can only contain < 10 element" data))
    (when (not (andmap integer? data))
      (error 'scribbler-send "~s can only contain integers" data))
    (let ([msg (string-append (list->string (map integer->char data))
                 (make-string (- 9 (length data)) #\space))])
      (block-write out msg 9))))

(define scribbler-read
  (lambda (in bytes)
    (if (scribbler-debug?)
        (printf "scribbler-read: Last command echo: ~a~n" (fluke-read in 9))
        (iterate! (for i (up-from 0 (to 9))) (read-char in)))
    (fluke-read in bytes)))

(define fluke-read
  (lambda (in count)
    (collect-vector-of-length count (for i (up-from 0 (to count)))
      (read-char in))))

(define write-number/bytes
  (lambda (n c port)
    (let ([bytes (number->bytes n)])
      (unless (>= c (length bytes))
        (error 'write-number/bytes 
          "~a cannot be written using only ~a bytes" n c))
      (loop ([for i (up-from 0 (to (- c (length bytes))))])
        (display #\nul port))
      (for-each (lambda (byte) (display byte port)) bytes))))

(define number->bytes
  (lambda (n)
    (if (zero? n) '(#\nul)
      (collect-list-reverse ([with n n (quotient n 255)]
                             [let piece (modulo n 256)]
                             [until (zero? n)])
        (integer->char piece)))))

(define read-bytes
  (lambda (file)
    (collect-vector (for value (in-file file read))
      (integer->char value))))

(define read-bytes->number
  (lambda (in count)
    (let ([bytes (collect-list (for i (up-from 0 (to count))) (read-char in))])
      (fold-left (lambda (s e) (logxor (ash s 8) (char->integer e)))
        0 bytes))))

(define unhexlify
  (lambda (s)
    (define high-value
      (lambda (s i) (string->number (string (string-ref s i)) 16)))
    (define low-value (lambda (s i) (high-value s (1+ i))))
    (unless (even? (string-length s))
      (error 'unhexlify
        "String does not contain a valid set of character pairs"))
    (collect-vector-of-length (/ (string-length s) 2)
        ([for i (up-from 0 (to (string-length s)) (by 2))]
         [let high (high-value s i)]
         [let low (low-value s i)])
      (+ (* high 16) low))))

(define ih-decode
  (lambda (buf offset line)
    (unless (char=? #\: (string-ref line 0))
      (error 'ih-decode "Not a valid hex record: ~a" line))
    (let* ([bin (unhexlify (string-drop line 1))]
           [len (vector-length bin)])
      (unless (<= 5 len)
        (error 'ih-decode "Not a valid hex record: ~a" line))
      (let ([rec-len (vector-ref bin 0)]
            [rec-type (vector-ref bin 3)]
            [addr (+ (vector-ref bin 2) (* 256 (vector-ref bin 1)))])
        (unless (= len (+ 5 rec-len))
          (error 'ih-decode "Bad record length: ~a vs. ~a" (+ 5 rec-len) len))
        (unless (zero? (logand #x0FF (collect-sum (for e (in-vector bin)) e)))
          (error 'ih-decode "Checksum mismatch"))
        (%ih-decode buf offset bin rec-len rec-type addr)))))

(define %ih-decode
  (lambda (buf offset bin rec-len rec-type addr)
    (case rec-type
      [(0) (%ih-decode-data offset buf bin (+ offset addr) rec-len)]
      [(1) (%ih-decode-eof buf rec-len)]
      [(2) (%ih-decode-ext8086 buf bin addr rec-len)]
      [(3) (%ih-decode-start-seg offset buf bin addr rec-len)]
      [(4) (%ih-decode-extlin buf bin addr rec-len)]
      [(5) (%ih-decode-start-lin offset buf bin addr rec-len)]
      [else (error 'ih-decode "Invalid record type: ~a" rec-type)])))

(define %ih-decode-data
  (lambda (offset buf bin addr rec-len)
    (loop ([for val (in-vector bin 4 (+ 4 rec-len))]
           [for addr (up-from addr)]
           [with buf buf (ih-buffer-insert buf addr val)])
      => (values buf offset))))

(define %ih-decode-eof
  (lambda (buf rec-len)
    (unless (zero? rec-len)
      (error 'ih-decode "Invalid EOF Record"))
    (values buf (eof-object))))

(define %ih-decode-ext8086
  (lambda (buf bin addr rec-len)
    (unless (and (= rec-len 2) (zero? addr))
      (error 'ih-decode "Bad Extended 8086 Segment Record"))
    (values buf
      (* 16 (+ (vector-ref bin 5) (* 256 (vector-ref bin 4)))))))

(define %ih-decode-start-seg
  (lambda (offset buf bin addr rec-len)
    (unless (and (= 4 rec-len) (zero? addr))
      (error 'ih-decode "Bad Start Segment Address Record"))
    (values (ih-buffer-insert buf 'start-addr
              `((cs . ,(+ (vector-ref bin 5) (* 256 (vector-ref bin 4))))
                (ip . ,(+ (vector-ref bin 7) (* 256 (vector-ref bin 6))))))
      offset)))

(define %ih-decode-extlin
  (lambda (buf bin addr rec-len)
    (unless (and (= 2 rec-len) (zero? addr))
      (error 'ih-decode "Bad Extended Linear Address Record"))
    (values buf (* 65536 (+ (vector-ref bin 5) (* 256 (vector-ref bin 4)))))))
  
(define %ih-decode-start-lin
  (lambda (offset buf bin addr rec-len)
    (unless (and (= 4 rec-len) (zero? addr))
      (error 'ih-decode "Bad Start Linear Address Record"))

    (values (ih-buffer-insert buf 'start-addr
              `((EIP . (+ (vector-ref bin 7)
                         (* 256 (vector-ref bin 6))
                         (* 65536 (vector-ref bin 5))
                         (* 16777216 (vector-ref bin 4))))))
      offset)))

(define ih-buffer-insert
  (lambda (buf addr value)
    (when (assq addr buf)
      (error 'ih-buffer-insert "~a is already filled" addr))
    (cons (cons addr value) buf)))

(define empty-ih-buffer '())

;; FIXME: Very slow buffer representation
(define load-intelhex
  (lambda (file)
    (define crlf-readline (lambda (port) (read-line port 'crlf)))
    (loop ([for line (in-file file crlf-readline)]
           [with buf empty-ih-buffer new-buf]
           [with offset 0 new-offset]
           [let-values (new-buf new-offset) (ih-decode buf offset line)]
           [until (eof-object? offset)])
      => buf)))

(define intelhex->vector
  (lambda (buf)
    (let ([high-addr (intelhex-max-addr buf)]
          [low-addr (intelhex-min-addr buf)]
          [padding #x0FF])
      (let* ([vec-size (- high-addr (1- low-addr))]
             [vec (make-vector vec-size padding)])
        (iterate! (for entry (in-list buf))
          (when (integer? (car entry))
            (vector-set! vec (- (car entry) low-addr) (cdr entry))))
        vec))))

(define intelhex-max-addr
  (lambda (buf)
    (collect-maximum (for elem (in-list buf))
                     (if (integer? (car elem)))
      (car elem))))

(define intelhex-min-addr
  (lambda (buf)
    (collect-minimum (for elem (in-list buf))
                     (if (integer? (car elem)))
      (car elem))))

(define fluke-store-in-eeprom
  (lambda (in out data)
    (let* ([len (vector-length data)]
           [segs (ceiling (/ len 264))])
      (write-number/bytes segs 2 out)
      (printf "segs->~s~n" segs)
      (flush-output-port out)
      (iterate! (for page (up-from 0 (to segs)))
        (begin
          (printf "Sending page ~a...~n" (/ (* page 100) segs))
          (fluke-sendpage in out page data))))))

(define fluke-sendpage
  (lambda (in out page data)
    (define write-range
      (lambda (segment)
        (collect-sum ([for i (up-from 0 (to 133))]
                      [let val (+ i (* 132 segment) (* 264 page))])
          (if (>= val (vector-length data))
              (begin (display #\nul out)
                (printf "~a,~a->~s~%" i val #\nul)
                0)
              (begin (display (integer->char (vector-ref data val)) out)
                (printf "~a,~a->~s~n" i val
                        (integer->char (vector-ref data val)))
                val)))))
    (call-with-output-file "test"
      (lambda (port)
        (vector-for-each (lambda (e) (format port "~s~n" (integer->char e)))
          data))
      'replace)
    (printf "DATA Length: ~a~n" (vector-length data))
    (loop cont ([with segment 0]
                [let sum (write-range segment)]
                [while (< segment (/ 264 132))])
      (display (integer->char (modulo sum 256)) out)
      (printf "sum->~s~n" (integer->char (modulo sum 256)))
      (flush-output-port out)
      (let ([c (read-char in)])
        (printf "Sum: ~a Return: ~a~n" (modulo sum 256) (char->integer c))
        (if (char=? #\* c)
            (cont (=> segment (1+ segment)))
            (cont))))))

(define light-value
  (lambda (vals)
    (list (fxlogor (fxsll (vector-ref vals 2) 8) (vector-ref vals 3))
          (fxlogor (fxsll (vector-ref vals 4) 8) (vector-ref vals 5))
          (fxlogor (fxsll (vector-ref vals 6) 8) (vector-ref vals 7)))))

(define ir-value
  (lambda (vals)
    (list (vector-ref vals 0) (vector-ref vals 1))))

(define line-value
  (lambda (vals)
    (list (vector-ref vals 8) (vector-ref vals 9))))

(define stall-value
  (lambda (vals)
    (vector-ref vals 10)))

(define %get-obstacle
  (lambda (in out direction)
    (display (integer->char (obstacle-dir-code direction)) out)
    (flush-output-port out)
    (read-bytes->number in 2)))

(define obstacle-dir-code
  (lambda (dir)
    (case dir 
      [(left) code/get-dongle-left-ir]
      [(center) code/get-dongle-center-ir]
      [(right) code/get-dongle-right-ir]
      [else (error 'get-obstacle "Bad direction ~a" dir)])))

(define %get-bright
  (lambda (bot direction)
    (let ([out (scribbler-out bot)]
          [in (scribbler-in bot)]
          [window (bright-direction-code direction)])
      (display (integer->char code/get-window-light) out)
      (display (integer->char window) out)
      (flush-output-port out)
      (read-bytes->number in 3))))

(define bright-direction-code
  (lambda (direction)
    (case direction 
      [(left) 0] 
      [(middle center) 1]
      [(right) 2]
      [else (error 'get-bright "Bad direction ~a" direction)])))

(define %translate
  (lambda (bot speed interval)
    (move bot speed 0)
    (unless (zero? interval)
      (sleep interval)
      (stop bot))))

(define %rotate
  (lambda (bot speed interval)
    (move bot 0 speed)
    (unless (zero? interval)
      (sleep interval)
      (stop bot))))

(define read-serial-memory
  (lambda (in out page offset)
    (display (integer->char code/get-serial-mem) out)
    (write-number/bytes page 2 out)
    (write-number/bytes offset 2 out)
    (flush-output-port out)
    (read-char in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Byte Codes

(define code/get-battery 89)
(define code/get-scribbler-program 91)
(define code/set-loud 111)
(define code/set-quiet 112)
(define code/set-speaker 113)
(define code/set-speaker2 114)
(define code/set-scribbler-program 122)
(define code/set-start-program 123)
(define code/get-info 80)
(define code/upgrade-fluke-sucess 42)
(define code/upgrade-fluke-error 1)
(define code/upgrade-fluke-segment-size 132)
(define code/upgrade-fluke 40)
(define code/get-all 65)
(define code/get-dongle-left-ir 85)
(define code/get-dongle-center-ir 86)
(define code/get-dongle-right-ir 87)
(define code/get-window-light 88)
(define code/set-window 127)
(define code/get-blob 95)
(define code/set-motors-off 108)
(define code/set-motors 109)
(define code/get-name1 78)
(define code/get-name2 64)
(define code/get-password1 50)
(define code/get-password2 51)
(define code/get-data 81)
(define code/set-led-left-on 99)
(define code/set-led-left-off 100)
(define code/set-led-center-on 101)
(define code/set-led-center-off 102)
(define code/set-led-right-on 103)
(define code/set-led-right-off 104)
(define code/set-led-all-on 105)
(define code/set-led-all-off 106)
(define code/set-dongle-led-on 116)
(define code/set-dongle-led-off 117)
(define code/set-dimmer-led 126)
(define code/set-name1 110)
(define code/set-name2 119)
(define code/set-data 97)
(define code/set-dongle-ir 120)
(define code/get-serial-mem 90)
(define code/set-forwardness 128)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Load the System Specific Code
(define-syntax load-system-file
  (lambda (x)
    (syntax-case x ()
      [(k)
       (let* ([mt (symbol->string (machine-type))]
              [file (string-append "sys" (string (directory-separator))
                                   mt ".ss")])
         #`(#,(datum->syntax #'k 'include) #,file))])))

(load-system-file)

)
