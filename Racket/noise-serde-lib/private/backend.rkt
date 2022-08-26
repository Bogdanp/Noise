#lang racket/base

(require (for-syntax racket/base
                     racket/syntax-srcloc
                     syntax/parse)
         "sequencer.rkt"
         "serde.rkt")

(provide
 define-rpc :

 rpc-infos
 (struct-out rpc-arg)
 (struct-out rpc-info))

(define (normalize t)
  (if (record-info? t)
      (Untagged t)
      t))

(struct rpc-arg (label name type))
(struct rpc-info ([id #:mutable] name args response-type proc))

(define rpc-infos (make-hasheqv))
(define rpc-info-sequencer
  (make-sequencer
   rpc-infos
   rpc-info-name
   set-rpc-info-id!))

(define-syntax (: stx)
  (raise-syntax-error ': "may only be used within a define-rpc form" stx))

(define-syntax known-names
  (make-hasheq))

(begin-for-syntax
  (define (valid-name? id-stx)
    (define id-str (symbol->string (syntax-e id-stx)))
    (regexp-match? #rx"^[_a-zA-Z][_a-zA-Z0-9-]*$" id-str))

  (define-syntax-class arg
    #:literals (:)
    (pattern [name:id : type:expr] #:with label #'_)
    (pattern [label:id name:id : type:expr])))

(define-syntax (define-rpc stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (name:id arg:arg ... : type:expr) body ...+)
     #:fail-unless (valid-name? #'name)
     "RPC names may only contain alphanumeric characters, dashes and underscores"
     #:fail-unless (andmap valid-name? (syntax-e #'(arg.name ...)))
     "RPC labels may only contain alphanumeric characters, dashes and underscores"
     (define name-sym (syntax-e #'name))
     (define known-names (syntax-local-value #'known-names))
     (when (hash-has-key? known-names name-sym)
       (define other-stx (hash-ref known-names name-sym))
       (define other-loc (srcloc->string (syntax-srcloc other-stx)))
       (define other-message (format "~n  existing definition: ~a" other-loc))
       (define message (format "cannot redefine RPC ~a" name-sym))
       (raise-syntax-error 'define-rpc message stx #'name null other-message))
     (hash-set! known-names name-sym stx)
     #'(begin
         (define response-type (normalize type))
         (unless (field-type? response-type)
           (error 'define-rpc "~e is not a valid response type~n in: ~a" response-type 'name))
         (define (name arg.name ...)
           body ...)
         (define arg-types
           (for/list ([n (in-list (list 'arg.name ...))]
                      [t (in-list (list arg.type ...))])
             (define normalized-t
               (normalize t))
             (begin0 normalized-t
               (unless (field-type? normalized-t)
                 (error 'define-rpc "~e is not a valid field type~n in arg: ~a" t n)))))
         (define args
           (for/list ([l (in-list (list 'arg.label ...))]
                      [n (in-list (list 'arg.name ...))]
                      [t (in-list arg-types)])
             (rpc-arg l n t)))
         (define info
           (rpc-info #f 'name args response-type name))
         (sequencer-add! rpc-info-sequencer info))]))
