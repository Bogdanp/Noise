#lang info

(define license 'BSD-3-Clause)
(define version "0.6")
(define collection "noise")
(define deps '("base"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
(define raco-commands
  '(("noise-serde-codegen" (submod noise/codegen main) "generate Noise ser/de code" #f)))
