#lang racket/base

(require racket/match)

(provide
 make-sequencer
 sequencer-add!)

(struct sequencer (ht name-proc [names #:mutable] set-id!-proc))

(define (make-sequencer ht name-proc set-id!-proc)
  (sequencer ht name-proc null set-id!-proc))

(define (sequencer-add! s v)
  (match-define (sequencer ht name-proc names set-id!-proc) s)
  (define name (name-proc v))
  (unless (symbol? name)
    (raise-argument-error 'sequencer-next! "(symbol? (name-proc v))" v))
  (when (memq name names)
    (error 'sequencer-next! "duplicate name ~s" name))
  (define new-names
    (sort (cons name names) symbol<?))
  (set-sequencer-names! s new-names)
  (define new-ids
    (for/hasheq ([name (in-list new-names)]
                 [id (in-naturals)])
      (values name id)))
  (define vs (hash-values ht))
  (hash-clear! ht)
  (for ([v (in-list (cons v vs))])
    (define id (hash-ref new-ids (name-proc v)))
    (set-id!-proc v id)
    (hash-set! ht id v)))

(module+ test
  (require rackunit)
  (define ht (make-hasheqv))
  (define seq (make-sequencer ht values void))
  (for ([idx (in-range 1000)])
    (sequencer-add! seq (string->symbol (format "~a" idx))))
  (check-equal? (hash-ref ht 999) '|999|))
