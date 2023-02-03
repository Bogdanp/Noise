#lang racket/base

(provide
 valid-name-stx?)

(define (valid-name-stx? id-stx)
  (define id-str (symbol->string (syntax-e id-stx)))
  (regexp-match? #rx"^[_a-zA-Z][_a-zA-Z0-9-]*$" id-str))
