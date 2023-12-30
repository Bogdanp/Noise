#lang racket/base

(require noise/backend
         noise/serde)

(define-rpc (echo [s : String] : String)
  (void))
