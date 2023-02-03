#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "common.rkt")
         ffi/unsafe
         racket/port
         "../unsafe/callout.rkt"
         "sequencer.rkt"
         "serde.rkt")

(provide
 define-callout

 callout-infos
 (struct-out callout-arg)
 (struct-out callout-info))

(struct callout-arg (name type))
(struct callout-info ([id #:mutable] name args cbox))

(define callout-infos (make-hasheqv))
(define callout-info-sequencer
  (make-sequencer
   callout-infos
   callout-info-name
   set-callout-info-id!))

(define-syntax (define-callout stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (name:id [arg-name:id : arg-type:expr] ...+))
     #:fail-unless (valid-name-stx? #'name)
     "callout names may only contain alphanumeric characters, dashes and underscores"
     #'(begin
         (define cbox
           (make-callout-box callout-type))
         (define (name arg-name ...)
           (do-callout info (list (cons (->field-type 'Callout arg-type) arg-name) ...)))
         (define args
           (for/list ([n (in-list (list 'arg-name ...))]
                      [t (in-list (list arg-type ...))])
             (callout-arg n (->field-type 'Callout t))))
         (define info
           (callout-info #f 'name args cbox))
         (sequencer-add! callout-info-sequencer info))]))

(define callout-type
  (_fun _int _size _bytes -> _void))

(define (do-callout info arg-pairs)
  (define id (callout-info-id info))
  (define cbox (callout-info-cbox info))
  (define bs
    (call-with-output-bytes
     (lambda (out)
       (for ([p (in-list arg-pairs)])
         (write-field (car p) (cdr p) out)))))
  (cbox id (bytes-length bs) bs))
