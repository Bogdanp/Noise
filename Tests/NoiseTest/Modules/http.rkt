#lang racket/base

(require (prefix-in http: net/http-easy))

(provide get)

(define (get uri)
  (define res (http:get uri))
  (values
   (http:response-status-code res)
   (http:response-body res)))
