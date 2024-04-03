#lang info

(define license 'BSD-3-Clause)
(define collection "noise")
(define deps
  '("base"
    "review"))
(define review-exts
  '([noise/review should-review-syntax? review-syntax]))
