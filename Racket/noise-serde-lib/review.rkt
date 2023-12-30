#lang racket/base

#|review: ignore|#

(require review/ext
         syntax/parse/pre)

(provide
 should-review-syntax?
 review-syntax)

(define-expression-syntax-class expression)

(define (should-review-syntax? stx)
  (syntax-case stx (define-rpc)
    [(define-rpc . _rest) #t]
    [_ #f]))

(define-syntax-class rpc-arg
  #:datum-literals (:)
  (pattern [{~optional _} id:id : type-expr:expression]
           #:do [(track-binding #'id)]))

(define-syntax-class rpc-definition
  #:datum-literals (define-rpc :)
  (pattern (define-rpc
             ~!
             {~do (push-scope)}
             (name:id arg:rpc-arg ... {~optional {~seq : type-expr:expression}})
             {~do (push-scope)}
             body-e:expression ...)
           #:do [(pop-scope)
                 (pop-scope)
                 (track-binding #'name #:check-usages? #f)]))

(define (review-syntax stx)
  (syntax-parse stx
    [d:rpc-definition #'d]))
