#lang racket/base

(require ffi/unsafe)

(provide
 callout-box?
 make-callout-box
 callout-box-install!)

(struct callout-box (type [proc #:mutable])
  #:property prop:procedure (λ (b . args)
                              (define proc (callout-box-proc b))
                              (unless proc
                                (error 'callout-box "procedure not installed"))
                              (apply (callout-box-proc b) args)))

(define (make-callout-box type)
  (callout-box type #f))

(define (callout-box-install! b addr)
  (set-callout-box-proc! b (cast addr _intptr (callout-box-type b))))
