#lang info

(define version "0.2")
(define collection "noise")
(define deps '("base"))
(define build-deps '("rackunit-lib"))
(define raco-commands
  '(("noise-serde-codegen" (submod noise/codegen main) "generate Noise ser/de code" #f)))
