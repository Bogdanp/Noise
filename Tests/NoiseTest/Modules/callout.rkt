#lang racket/base

(require ffi/unsafe
         noise/unsafe/callout)

(provide
 install-callout!
 exec-callout)

(define callout
  (make-callout-box (_fun _int _bytes -> _void)))

(define (install-callout! addr)
  (callout-box-install! callout addr))

(define (exec-callout)
  (callout 5 #"hello"))
