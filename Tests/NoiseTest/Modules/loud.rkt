#lang racket/base

(require racket/format)

(provide exclaim)

(define (exclaim s)
  (string->bytes/utf-8 (~a s "!!!")))
