#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract
         racket/generic
         racket/port)

;; record ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 define-record
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
  (apply
   (record-info-constructor r)
   (for/list ([f (in-list (record-info-fields r))])
     (read-field (record-field-type f) in))))

(define (do-write-record r v [out (current-output-port)])
  (write-field UVarint (record-info-id r) out)
  (for ([f (in-list (record-info-fields r))])
    (write-field (record-field-type f) ((record-field-accessor f) v) out)))

(provide
 record-infos
 (struct-out record-info)
 (struct-out record-field))

(define record-infos (make-hasheqv))
(struct record-info (id name constructor fields))
(struct record-field (id type accessor))

(define-syntax known-records (make-hasheq))

(define-syntax (define-record stx)
  (define (id-stx->keyword stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

  (define-syntax-class record-field
    (pattern [id:id ft:expr ctc:expr]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'id
             #:with opt? #f)
    (pattern [(id:id def:expr) ft:expr ctc:expr]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'[id def]
             #:with opt? #t))

  (define known (syntax-local-value #'known-records))
  (syntax-parse stx
    [(_ name:id id:number fld:record-field ...)
     #:fail-unless (exact-nonnegative-integer? (syntax-e #'id))
     "guid must be a positive integer"
     #:fail-when (hash-has-key? known (syntax-e #'id))
     (let ([other (hash-ref known (syntax-e #'id))])
       (format "record ~a (defined in \"~a\") already has guid ~a"
               (car other) (syntax-source (cadr other)) (syntax-e #'id)))
     #:with id? (format-id #'name "~a?" #'name)
     #:with record-id (format-id #'name "record:~a" #'name)
     #:with constructor-id (format-id #'name "make-~a" #'name)
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
     #:with (accessor-id ...) (for/list ([fld (in-list (syntax-e #'(fld.id ...)))])
                                (format-id fld "~a-~a" #'name fld))
     (hash-set! known (syntax-e #'id) (list (syntax-e #'name) stx))
     #'(begin
         (struct name (fld.id ...) #:transparent
           #:methods gen:record
           [(define (write-record self [out (current-output-port)])
              (do-write-record record-id self out))])
         (define record-id
           (record-info id 'name name (list (record-field 'fld.id fld.ft accessor-id) ...)))
         (hash-set! record-infos id record-id)
         (define/contract (constructor-id constructor-arg ...)
           (->* (required-ctor-arg-ctc ...)
                (optional-ctor-arg-ctc ...)
                id?)
           (name fld.id ...)))]))

(module+ test
  (require racket/port
           rackunit)

  (test-case "record serde"
    (define-record Human #xFF10
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

 (struct-out field-type))

(struct field-type (read-proc write-proc swift-proc))

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
     (format "[~a]" swift-type))))

(define-field-type Record
  #:read read-record
  #:write write-record)

(module+ test
  (test-case "complex field serde"
    (define-record Example #xFF01
      [b Bool boolean?]
      [i Varint integer?]
      [s String string?]
      [l (Listof Varint) list?])
    (define v (Example #t -1 "hello" '(0 1 2 #x-FF #x7F #xFFFF)))
    (define bs (with-output-to-bytes (λ () (write-field Record v))))
    (check-equal? v (read-field Record (open-input-bytes bs)))))
