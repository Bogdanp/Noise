#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax
                     syntax/parse)
         racket/contract
         racket/generic
         racket/port
         "sequencer.rkt")

;; record ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 define-record
 record-out
 record?
 read-record
 write-record)

(define-generics record
  {write-record record [out]})

(define (read-record [in (current-input-port)])
  (define id (read-field UVarint in))
  (unless (hash-has-key? record-infos id)
    (error 'read-record "unknown record type ~a" id))
  (define r (hash-ref record-infos id))
  (do-read-record-fileds r in))

(define (do-write-record r v [out (current-output-port)])
  (write-field UVarint (record-info-id r) out)
  (do-write-record-fields r v out))

(define (do-read-record-fileds r in)
  (apply
   (record-info-constructor r)
   (for/list ([f (in-list (record-info-fields r))])
     (read-field (record-field-type f) in))))

(define (do-write-record-fields r v out)
  (for ([f (in-list (record-info-fields r))])
    (write-field (record-field-type f) ((record-field-accessor f) v) out)))

(provide
 record-infos
 (struct-out record-info)
 (struct-out record-field))

(struct record-info ([id #:mutable] name constructor fields)
  #:property prop:procedure (struct-field-index constructor))
(struct record-field (id type accessor))

(define record-infos (make-hasheqv))
(define record-info-sequencer
  (make-sequencer
   record-infos
   record-info-name
   set-record-info-id!))

(define-syntax (define-record stx)
  (define (id-stx->keyword stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

  (define-syntax-class record-field
    (pattern [id:id ft:expr]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'id
             #:with opt? #f
             #:with ctc #'any/c)
    (pattern [id:id ft:expr ctc:expr]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'id
             #:with opt? #f)
    (pattern [(id:id def:expr) ft:expr]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'[id def]
             #:with opt? #t
             #:with ctc #'any/c)
    (pattern [(id:id def:expr) ft:expr ctc:expr]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'[id def]
             #:with opt? #t))

  (syntax-parse stx
    [(_ name:id fld:record-field ...)
     #:with name? (format-id #'name "~a?" #'name)
     #:with constructor-id (format-id #'name "make-~a" #'name)
     #:with (fld-accessor-id ...) (for/list ([arg (in-list (syntax-e #'(fld.id ...)))])
                                    (format-id #'name "~a-~a" #'name arg))
     #:with (fld-setter-id ...) (for/list ([arg (in-list (syntax-e #'(fld.id ...)))])
                                  (format-id #'name "set-~a-~a" #'name arg))
     #:with (constructor-arg ...) (apply
                                   append
                                   (for/list ([kwd (in-list (syntax-e #'(fld.kwd ...)))]
                                              [arg (in-list (syntax-e #'(fld.arg ...)))])
                                     (list kwd arg)))
     #:with (required-ctor-arg-ctc ...) (apply
                                         append
                                         (for/list ([opt? (in-list (syntax->datum #'(fld.opt? ...)))]
                                                    [kwd  (in-list (syntax-e #'(fld.kwd ...)))]
                                                    [ctc  (in-list (syntax-e #'(fld.ctc ...)))]
                                                    #:unless opt?)
                                           (list kwd ctc)))
     #:with (optional-ctor-arg-ctc ...) (apply
                                         append
                                         (for/list ([opt? (in-list (syntax->datum #'(fld.opt? ...)))]
                                                    [kwd  (in-list (syntax-e #'(fld.kwd ...)))]
                                                    [ctc  (in-list (syntax-e #'(fld.ctc ...)))]
                                                    #:when opt?)
                                           (list kwd ctc)))
     #'(begin
         (define-syntax (name stx-or-mode)
           (case stx-or-mode
             [(provide)
              #'(combine-out name name? constructor-id fld-accessor-id ... fld-setter-id ...)]
             [else
              (syntax-case stx-or-mode ()
                [(_ arg (... ...)) #'(-name arg (... ...))]
                [id:id #'info])]))
         (define-values (-name name? fld-accessor-id ... fld-setter-id ...)
           (let ()
             (struct name (fld.id ...)
               #:transparent
               #:methods gen:record
               [(define (write-record self [out (current-output-port)])
                  (do-write-record info self out))])
             (define/contract (fld-setter-id r v)
               (-> name? fld.ctc name?)
               (struct-copy name r [fld.id v])) ...
             (values name name? fld-accessor-id ... fld-setter-id ...)))
         (define info
           (let ([fields (list (record-field 'fld.id
                                             (let ([ft fld.ft])
                                               (if (record-info? ft)
                                                   (Untagged ft)
                                                   ft))
                                             fld-accessor-id)
                               ...)])
             (record-info #f 'name -name fields)))
         (sequencer-add! record-info-sequencer info)
         (define/contract (constructor-id constructor-arg ...)
           (->* (required-ctor-arg-ctc ...)
                (optional-ctor-arg-ctc ...)
                name?)
           (-name fld.id ...)))]))

(define-syntax record-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ id)
        (define export-stx
          ((syntax-local-value #'id) 'provide))
        (expand-export export-stx modes)]))))

(module+ test
  (require racket/port
           rackunit)

  (test-case "record serde"
    (define-record Human
      [name String string?]
      [age Varint (integer-in 0 100)])
    (define h (make-Human #:name "Bogdan" #:age 30))
    (define bs (with-output-to-bytes (λ () (write-record h))))
    (check-equal? h (read-record (open-input-bytes bs)))))


;; varint ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 read-uvarint
 write-uvarint)

(define (write-uvarint v [out (current-output-port)])
  (define bs
    (let loop ([bs null] [n v])
      (define-values (q r)
        (quotient/remainder n #x80))
      (if (zero? q)
          (apply bytes (reverse (cons r bs)))
          (loop (cons (bitwise-ior r #x80 r) bs) q))))
  (write-bytes bs out))

(define (read-uvarint [in (current-input-port)])
  (let loop ([s 0])
    (define b (read-byte in))
    (if (zero? (bitwise-and b #x80))
        (arithmetic-shift b s)
        (+ (arithmetic-shift (bitwise-and b #x7F) s)
           (loop (+ s 7))))))

(define (write-varint v [out (current-output-port)])
  (write-uvarint
   (bitwise-xor
    (arithmetic-shift v 1)
    (if (< v 0) -1 0))
   out))

(define (read-varint [in (current-input-port)])
  (define n (read-uvarint in))
  (if (zero? (bitwise-and n 1))
      (arithmetic-shift n -1)
      (bitwise-not (arithmetic-shift n -1))))


;; field ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 Listof
 Untagged

 (struct-out field-type)
 read-field
 write-field)

(struct field-type
  (read-proc write-proc swift-proc))

(define (read-field t [in (current-input-port)])
  ((field-type-read-proc t) in))

(define (write-field t v [out (current-output-port)])
  ((field-type-write-proc t) v out))

(define-syntax (define-field-type stx)
  (syntax-parse stx
    [(_ id:id
        {~alt
         {~optional {~seq #:read read-expr:expr}}
         {~optional {~seq #:write write-expr:expr}}
         {~optional {~seq #:swift swift-expr:expr}}} ...)
     #'(begin
         (define id (field-type read-expr write-expr (~? swift-expr (λ () (symbol->string 'id)))))
         (provide id))]))

(define-field-type Bool
  #:read (λ (in)
           (= (read-byte in) 1))
  #:write (λ (b out)
            (write-byte (if b 1 0) out)))

(define-field-type Bytes
  #:read (λ (in)
           (read-bytes (read-varint in) in))
  #:write (λ (bs out)
            (write-varint (bytes-length bs) out)
            (write-bytes bs out))
  #:swift (λ () "Data"))

(define-field-type String
  #:read (λ (in)
           (bytes->string/utf-8 (read-bytes (read-varint in) in)))
  #:write (λ (s out)
            (define bs (string->bytes/utf-8 s))
            (write-varint (bytes-length bs) out)
            (write-bytes bs out)))

(define-field-type Symbol
  #:read (λ (in)
           (define len (read-varint in))
           (string->symbol (bytes->string/utf-8 (read-bytes len in))))
  #:write (λ (s out)
            (define bs (string->bytes/utf-8 (symbol->string s)))
            (write-varint (bytes-length bs) out)
            (write-bytes bs out)))

(define-field-type Varint
  #:read read-varint
  #:write write-varint)

(define-field-type UVarint
  #:read read-uvarint
  #:write write-uvarint)

(define (Listof t)
  (let ([t (if (record-info? t)
               (Untagged t)
               t)])
    (unless (field-type? t)
      (raise-argument-error 'Listof "(or/c field-type? record-info?)" t))
    (define read-proc (field-type-read-proc t))
    (define write-proc (field-type-write-proc t))
    (define swift-type ((field-type-swift-proc t)))
    (field-type
     (λ (in)
       (for/list ([_ (in-range (read-varint in))])
         (read-proc in)))
     (λ (vs out)
       (write-varint (length vs) out)
       (for-each (λ (v) (write-proc v out)) vs))
     (λ ()
       (format "[~a]" swift-type)))))

(define-field-type Record
  #:read read-record
  #:write write-record)

(define (Untagged t)
  (unless (record-info? t)
    (raise-argument-error 'Untagged "record-info?" t))
  (field-type
   (λ (in) (do-read-record-fileds t in))
   (λ (v out) (do-write-record-fields t v out))
   (λ () (symbol->string (record-info-name t)))))

(module+ test
  (test-case "complex field serde"
    (define-record Example
      [b Bool boolean?]
      [i Varint integer?]
      [s String string?]
      [l (Listof Varint) list?])
    (define v (Example #t -1 "hello" '(0 1 2 #x-FF #x7F #xFFFF)))
    (define bs (with-output-to-bytes (λ () (write-field Record v))))
    (check-equal? v (read-field Record (open-input-bytes bs))))

  (test-case "homogeneous list serde"
    (define-record Story
      [id UVarint exact-integer?]
      [title String string?]
      [comments (Listof UVarint) (listof exact-integer?)])
    (define-record Stories
      [stories (Listof Story) (listof Story?)])
    (define v
      (Stories
       (list (Story 0 "a" null)
             (Story 1 "b" '(1 2 3)))))
    (define bs (with-output-to-bytes (λ () (write-field Record v))))
    (check-equal? v (read-field Record (open-input-bytes bs))))

  (test-case "nested record serde"
    (define-record A
      [s String])
    (define-record B
      [a A])
    (define v
      (B (A "test")))
    (define bs (with-output-to-bytes (λ () (write-field Record v))))
    (check-equal? v (read-field Record (open-input-bytes bs)))))
