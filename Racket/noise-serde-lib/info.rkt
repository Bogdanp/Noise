#lang info

(define version "0.5")
(define collection "noise")
(define deps '("base"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
(define raco-commands
  '(("noise-serde-codegen" (submod noise/codegen main) "generate Noise ser/de code" #f)))
