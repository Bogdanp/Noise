#lang racket/base

#|review: ignore|#

(require review/ext
         syntax/parse/pre)

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-case stx (define-enum define-record define-rpc)
    [(define-enum . _rest) #t]
    [(define-record . _rest) #t]
    [(define-rpc . _rest) #t]
    [_ #f]))

(define-syntax-class enum-definition
  #:datum-literals (define-enum :)
  (pattern (define-enum
             ~!
             enum-id:id
             [variant-id:id {field-id:id : field-type:expression} ...] ...)
           #:do [(track-binding #'enum-id #:check-usages? #f)
                 (track-binding #'enum-id "~a?" #:check-usages? #f)
                 (define enum-id-sym (syntax->datum #'enum-id))
                 (for ([variant-id-stx (in-list (syntax-e #'(variant-id ...)))])
                   (track-binding
                    variant-id-stx
                    #:related-to #'enum-id
                    #:check-usages? #f
                    (format "~a.~~a" enum-id-sym)))]))

(define-syntax-class record-field
  (pattern [id:id : type-expr:expression . opts])
  (pattern [(id:id default-expr:expression) : type-expr:expression . opts]))

(define-syntax-class record-definition
  #:datum-literals (define-record :)
  (pattern (define-record
             ~!
             {~or record-id:id (record-id:id : protocol-id:id ...+)}
             record-field:record-field ...+)
           #:do [(track-binding #'record-id #:check-usages? #f)
                 (track-binding #'record-id "~a?" #:check-usages? #f)
                 (track-binding #'record-id "make-~a" #:check-usages? #f)
                 (define record-id-sym (syntax->datum #'record-id))
                 (for ([record-field-id-stx (in-list (syntax-e #'(record-field.id ...)))])
                   (for ([p (in-list '("" "set-"))])
                     (track-binding
                      record-field-id-stx
                      #:related-to #'record-id
                      #:check-usages? #f
                      (format "~a~a-~~a" p record-id-sym))))]))

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
    [d:enum-definition #'d]
    [d:record-definition #'d]
    [d:rpc-definition #'d]))
