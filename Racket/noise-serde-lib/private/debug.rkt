#lang racket/base

(require racket/string)

(provide
 log-noise-debug
 make-debug-input-port
 make-debug-output-port)

(define-logger noise)

(define debug-ports?
  (getenv "NOISE_DEBUG_PORTS"))

(define (make-debug-input-port in)
  (if debug-ports? (do-make-debug-input-port in) in))

(define (make-debug-output-port out)
  (if debug-ports? (do-make-debug-output-port out) out))

(define (do-make-debug-input-port in)
  (make-input-port
   (object-name in)
   (lambda (out-bs)
     (define num-read
       (read-bytes-avail!* out-bs in))
     (begin0 (if (zero? num-read) in num-read)
       (if (eof-object? num-read)
           (log-noise-debug "read: eof")
           (log-noise-debug "read: ~a" (~hex-bytes (subbytes out-bs 0 num-read))))))
   (lambda (out-bs skip-n maybe-progress-evt)
     (define num-peeked
       (peek-bytes-avail!* out-bs skip-n maybe-progress-evt in))
     (begin0 (if (zero? num-peeked) in num-peeked)
       (if (eof-object? num-peeked)
           (log-noise-debug "peek: eof")
           (log-noise-debug "peek: ~a" (~hex-bytes (subbytes out-bs 0 num-peeked))))))
   (lambda ()
     (close-input-port in))))

(define (do-make-debug-output-port out)
  (make-output-port
   (object-name out)
   out
   (lambda (bs start-pos end-pos _block? enable-break?)
     (define num-written
       (if enable-break?
           (parameterize-break #t
             (write-bytes-avail* bs out start-pos end-pos))
           (write-bytes-avail* bs out start-pos end-pos)))
     (begin0 num-written
       (log-noise-debug "write: ~a" (~hex-bytes (subbytes bs start-pos (+ start-pos num-written))))))
   (lambda ()
     (close-output-port out))))

(define (~hex-bytes bs)
  (string-join
   (for/list ([b (in-bytes bs)])
     (define hex
       (number->string b 16))
     (if (< b 16)
         (format "0~a" hex)
         hex))
   " "))
