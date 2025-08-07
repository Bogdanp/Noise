#lang racket/base

(require racket/match
         racket/promise)

(provide
 make-sequencer)

(struct sequencer
  (name-proc
   [names #:mutable]
   set!-proc
   [finalized? #:mutable]))

(define (make-sequencer name-proc set!-proc)
  (define seq
    (sequencer
     #;name-proc name-proc
     #;names (hasheq)
     #;set!-proc set!-proc
     #;finalized? #f))
  (define ht-promise
    (delay/sync
     (finalize-sequencer seq)))
  (values
   (λ (v) (sequencer-add! seq v))
   (λ () (force ht-promise))))

(define (sequencer-add! s v)
  (match-define (sequencer name-proc names _ finalized?) s)
  (when finalized?
    (raise-user-error 'sequencer-add! "sequencer already finalized"))
  (define name (name-proc v))
  (unless (symbol? name)
    (raise-argument-error 'sequencer-next! "(symbol? (name-proc v))" v))
  (when (hash-has-key? names name)
    (error 'sequencer-next! "duplicate name ~s" name))
  (set-sequencer-names! s (hash-set names name v)))

(define (finalize-sequencer s)
  (match-define (sequencer _ names set!-proc _) s)
  (define sorted-names
    (sort (hash-keys names) symbol<?))
  (define ht
    (for/hasheq ([name (in-list sorted-names)]
                 [id (in-naturals)])
      (define v (hash-ref names name))
      (set!-proc v id)
      (values id v)))
  (set-sequencer-finalized?! s #t)
  ht)

(module+ test
  (require rackunit)
  (define-values (save! get-ht)
    (make-sequencer values void))
  (for ([idx (in-range 1000)])
    (save! (string->symbol (format "~a" idx))))
  (define ht (get-ht))
  (check-equal? (hash-ref ht 999) '|999|)
  (check-exn
   #rx"sequencer already finalized"
   (λ () (save! '|1000|))))
