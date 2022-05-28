#lang racket/base

(provide fib)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 2))
         (fib (- n 1)))))
