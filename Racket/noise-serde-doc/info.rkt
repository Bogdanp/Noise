#lang info

(define version "0.1")
(define collection "noise")
(define deps '("base"))
(define build-deps '("base"
                     "noise-serde-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("noise-serde-lib"))
(define scribblings '(("noise-manual.scrbl")))