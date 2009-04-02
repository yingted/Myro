;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows XP+ Connection Code for Scribblers
;;; 
;;; Copyright (c) 2009 Aaron Hsu <arcfide@sacrideo.us>
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

(module (sleep connect-scribbler)

(include "./lib/assert.ss")
(include "./lib/def-ext.ss")

(import assertions)
(import foof-loop)
(import nested-foof-loop)
(import extended-definitions)

;;; Some constants
(define-constant perms 3221225472) ;; GENERIC_READ | GENERIC_WRITE
(define-constant shared-mode 0)
(define-constant security 0)
(define-constant open-existing 3)
(define-constant flag-attribs 0)
(define-constant template 0)
(define-constant read-write-perms 514) ;; O_RDWR | O_BINARY
	
;;; Handle Type
(define-record handle (val))

(define $create-file
  (foreign-procedure __stdcall "CreateFileA"
    (string unsigned-32 unsigned-32 unsigned-32 
     unsigned-32 unsigned-32 unsigned-32)
    fixnum))

(define $close-handle 
  (foreign-procedure __stdcall "CloseHandle" (fixnum) boolean))
  
(define $sleep 
  (foreign-procedure __stdcall "Sleep" (fixnum) void))
  
(define $get-last-error
  (foreign-procedure __stdcall "GetLastError" () unsigned-32))

;;; Custom Glue
(define $chars-waiting 
  (foreign-procedure "_chars_waiting" (integer-32) unsigned-32))

(define $read-file
  (foreign-procedure "_read_file"
    (integer-32 string unsigned-32)
    integer-32))

(define $write-file
  (foreign-procedure "_write_file"
    (integer-32 string unsigned-32)
    integer-32))
  
(define win32-read-file
  (lambda (handle buffer count)
    (assert (<= count (string-length buffer)))
	(let ([bytes-read ($read-file (handle-val handle) buffer count)])
	  (case bytes-read
	    [(0) (eof-object)]
	    [(-1) (error #f "Error reading from ~s" handle)]
	    [else bytes-read]))))

(define win32-write-file
  (lambda (handle buffer count)
    (assert (<= count (string-length buffer)))
	(let ([bytes-written 
	       ($write-file (handle-val handle) buffer count)])
	  (case bytes-written
	    [(-1) (error #f "Error writing to ~s" handle)]
	    [else (void)]))))

;;; We really need to use a Mutex to ensure thread safety here, but 
;;; for now, let's just assume single threaded mode. This should be 
;;; changed when the rest of the scribbler code goes mutli-threaded.
(define make-win32-input-handler
  (lambda (handle)
    (let ([peeked? (box #f)] [open? #t])
      (lambda (message . args)
        (unless open? (error #f "Port for ~s is closed" handle))
        (case message
	  [(block-read) 
	   (apply %block-read 
	     `(,handle ,peeked? ,@args))]
	  [(char-ready?) #t]
	  [(clear-input-port)
	   (set-box! peeked? #f)
	   #;(%clear-buffer handle ($chars-waiting (handle-val handle)))]
	  [(close-port)
	   (set! open? #f)
	   (unless ($close-handle (handle-val handle))
	     (error #f "Error closing handle ~s" handle))]
	  [(file-position) (most-negative-fixnum)]
	  [(file-length) 0]
	  [(peek-char) 
	   (%peek-char handle peeked? (car args))]
	  [(port-name) #f]
	  [(read-char) 
	   (%read-char handle peeked? (car args))]
	  [else 
	    (error #f "~s does not support this message" handle)])))))

(define string-copy!
  (lambda (dst src ds de ss se)
    (iterate! ([for i (up-from ds (to (min de (string-length dst))))]
               [for j (up-from ss (to (min se (string-length src))))])
      (string-set! dst i (string-ref src j)))))

(define %block-read
  (lambda (handle peeked? port buf count)
    (assert (<= 1 count (string-length buf)))
    (if (unbox peeked?) 
        (let* ([size (1- (string-length buf))]
	       [tmp (make-string size)]
	       [act (win32-read-file handle tmp (1- count))])
	  (string-set! buf 0 (unbox peeked?))
	  (set-box! peeked? #f)
	  (string-copy! buf tmp 1 (1+ size) 0 act)
	  (1+ act))
	(win32-read-file handle buf count))))

#;(define %clear-buffer
  (lambda (handle count)
    (let ([s (make-string count)])
      (win32-read-file handle s count)
      (void))))

(define %peek-char
  (lambda (handle peeked? port)
    (or (unbox peeked?)
      (let* ([tmp (make-string 1)]
             [res (win32-read-file handle tmp 1)])
        (if (eof-object? res)
	    (eof-object)
	    (begin (set-box! peeked? (string-ref tmp 0))
	      (unbox peeked?)))))))

(define %read-char
  (lambda (handle peeked? port)
    (if (unbox peeked?)
        (let ([c (unbox peeked?)]) (set-box! peeked? #f) c)
	(let* ([tmp (make-string 1)]
	       [res (win32-read-file handle tmp 1)])
	  (if (eof-object? res)
	      (eof-object)
	      (string-ref tmp 0)))))) 

(define make-win32-output-handler
  (lambda (handle)
    (let ([open? #t])
      (lambda (message . args)
        (unless open? (error #f "~s port is closed" handle))
        (case message
	  [(block-write) 
	   (let ([buf (cadr args)] [count (caddr args)])
	     (assert (<= count (string-length buf)))
	     (win32-write-file handle buf count))]
	  [(clear-output-port) (void)]
	  [(close-port) 
	   (set! open? #f)
	   (unless ($close-handle (handle-val handle))
	     (error #f "Failure closing ~s" handle))]
	  [(file-position) (most-negative-fixnum)]
	  [(file-length) 0]
	  [(flush-output-port) (void)]
	  [(port-name) #f]
	  [(write-char)
	   (win32-write-file handle (string (car args)) 1)]
	  [else (error #f "Invalid message ~s" message)])))))

(define sleep
  (lambda (sec)
    ($sleep (exact (round (* 1000 sec))))))

(define grab-com-handle
  (lambda (name)
    (make-handle
      ($create-file 
        name 
	perms 
	shared-mode 
	security 
	open-existing 
	flag-attribs 
	template))))

;;; We establish the connection using CreateFileA and then create a 
;;; custom port or two to handle the actual reading and writing. 
;;; We want to use unbuffered reading and writing, so make sure to 
;;; do that.
(define connect-scribbler
  (lambda (addr)
    (let ([handle (grab-com-handle addr)])
      (when (= -1 (handle-val handle))
        (error 'connect-scribbler "Failed to connect to ~s" addr))
      (values 
        (make-input-port 
	  (make-win32-input-handler handle)
	  (make-string 1))
	(make-output-port
	  (make-win32-output-handler handle)
	  (make-string 1))
	void))))

)
