#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre
                     "common.rkt")
         "sequencer.rkt"
         "serde.rkt")

(provide
 define-rpc

 get-rpc-infos
 (struct-out rpc-arg)
 (struct-out rpc-info))

(struct rpc-arg (label name type))
(struct rpc-info ([id #:mutable] name args response-type proc))

(define-values (save-rpc-info! get-rpc-infos)
  (make-sequencer rpc-info-name set-rpc-info-id!))

(begin-for-syntax
  (define-syntax-class arg
    #:literals (:)
    (pattern [name:id : type:expr] #:with label #'name)
    (pattern [label:id name:id : type:expr])))

(define-syntax (define-rpc stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (name:id arg:arg ... {~optional {~seq : type:expr}}) body ...+)
     #:fail-unless (valid-name-stx? #'name)
     "RPC names may only contain alphanumeric characters, dashes and underscores"
     #:fail-unless (andmap valid-name-stx? (syntax-e #'(arg.name ...)))
     "RPC labels may only contain alphanumeric characters, dashes and underscores"
     #`(begin
         (define response-type
           {~? (->field-type 'Backend type) Void})
         (unless (field-type? response-type)
           (error 'define-rpc "~e is not a valid response type~n in: ~a" response-type 'name))
         (define (name arg.name ...)
           body ...)
         (define args
           (for/list ([l (in-list (list 'arg.label ...))]
                      [n (in-list (list 'arg.name ...))]
                      [t (in-list (list arg.type ...))])
             (rpc-arg l n (->field-type 'Backend t))))
         (define info
           (rpc-info #f 'name args response-type name))
         (save-rpc-info! info)
         #,(when (eq? (syntax-local-context) 'module)
             #'(module+ rpc (provide name))))]))
