#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax
                     syntax/parse/pre)
         racket/contract/base
         racket/contract/region
         racket/generic
         racket/performance-hint
         racket/port
         "sequencer.rkt")

;; : ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :)

(define-syntax (: stx)
  (raise-syntax-error ': "may only be used within a define-enum, define-record or define-rpc form" stx))


;; record ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 define-record
 record-out)

(define (read-record info [in (current-input-port)])
  (apply
   (record-info-constructor info)
   (for/list ([f (in-list (record-info-fields info))])
     (read-field (record-field-type f) in))))

(define (write-record info v [out (current-output-port)])
  (define last-field-id #f)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define message
                       (format "~a~n  while writing record field: ~a.~a"
                               (exn-message e)
                               (record-info-name info)
                               last-field-id))
                     (raise (exn:fail message (current-continuation-marks))))])
    (for ([f (in-list (record-info-fields info))])
      (define type (record-field-type f))
      (define value ((record-field-accessor f) v))
      (set! last-field-id (record-field-id f))
      (write-field type value out))))

(provide
 record-infos
 (struct-out record-info)
 (struct-out record-field))

(struct record-info ([id #:mutable] name constructor protocols fields)
  #:property prop:procedure (struct-field-index constructor))
(struct record-field (id type mutable? accessor))

(define record-infos (make-hasheqv))
(define record-info-sequencer
  (make-sequencer
   record-infos
   record-info-name
   set-record-info-id!))

(begin-for-syntax
  (define-syntax-class protocol
    (pattern protocol:id #:with e #''protocol)))

(define-syntax (define-record stx)
  (define (id-stx->keyword stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))
  (define (stx->bool-stx stx)
    (datum->syntax stx (and (syntax-e stx) #t)))

  (define-syntax-class record-field
    #:literals (:)
    (pattern [id:id : ft:expr {~alt {~optional {~and #:mutable mutable}}
                                    {~optional {~seq #:contract ctc-expr:expr}}} ...]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'id
             #:with ctc #'{~? ctc-expr any/c}
             #:with opt? #'#f
             #:with mut? (stx->bool-stx #'{~? mutable #f}))
    (pattern [(id:id def:expr) : ft:expr {~alt {~optional {~and #:mutable mutable}}
                                               {~optional {~seq #:contract ctc-expr:expr}}} ...]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'[id def]
             #:with ctc #'{~? ctc-expr any/c}
             #:with opt? #'#t
             #:with mut? (stx->bool-stx #'{~? mutable #f})))

  (syntax-parse stx
    #:literals (:)
    [(_ {~or name:id (name:id : proto:protocol ...+)}
        fld:record-field ...)
     #:with name? (format-id #'name "~a?" #'name)
     #:with constructor-id (format-id #'name "make-~a" #'name)
     #:with (fld-accessor-id ...)
     (for/list ([arg (in-list (syntax-e #'(fld.id ...)))])
       (format-id #'name "~a-~a" #'name arg))
     #:with (fld-setter-id ...)
     (for/list ([arg (in-list (syntax-e #'(fld.id ...)))])
       (format-id #'name "set-~a-~a" #'name arg))
     #:with (constructor-arg ...)
     (apply
      append
      (for/list ([kwd (in-list (syntax-e #'(fld.kwd ...)))]
                 [arg (in-list (syntax-e #'(fld.arg ...)))])
        (list kwd arg)))
     #:with (required-ctor-arg-ctc ...)
     (apply
      append
      (for/list ([opt? (in-list (syntax->datum #'(fld.opt? ...)))]
                 [kwd  (in-list (syntax-e #'(fld.kwd ...)))]
                 [ctc  (in-list (syntax-e #'(fld.ctc ...)))]
                 #:unless opt?)
        (list kwd ctc)))
     #:with (optional-ctor-arg-ctc ...)
     (apply
      append
      (for/list ([opt? (in-list (syntax->datum #'(fld.opt? ...)))]
                 [kwd  (in-list (syntax-e #'(fld.kwd ...)))]
                 [ctc  (in-list (syntax-e #'(fld.ctc ...)))]
                 #:when opt?)
        (list kwd ctc)))
     #'(begin
         (define-syntax (name stx-or-mode)
           (case stx-or-mode
             [(provide-record)
              #'(combine-out name name? constructor-id fld-accessor-id ... fld-setter-id ...)]
             [else
              (unless (syntax? stx-or-mode)
                (raise-syntax-error 'name "unexpected argument to transformer" stx-or-mode))
              (syntax-case stx-or-mode ()
                [(_ arg (... ...)) #'(-name arg (... ...))]
                [id (identifier? #'id) #'info])]))
         (define-values (-name name? fld-accessor-id ... fld-setter-id ...)
           (let ()
             (struct name (fld.id ...)
               #:transparent)
             (define/contract (fld-setter-id r v)
               (-> name? fld.ctc name?)
               (struct-copy name r [fld.id v])) ...
             (values name name? fld-accessor-id ... fld-setter-id ...)))
         (define info
           (let ([protocols {~? (list proto.e ...) null}]
                 [fields (list (record-field 'fld.id (->field-type 'Record fld.ft) fld.mut? fld-accessor-id) ...)])
             (record-info #f 'name -name protocols fields)))
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
          ((syntax-local-value #'id) 'provide-record))
        (expand-export export-stx modes)]))))

(module+ test
  (require racket/port
           rackunit)

  (test-case "record serde"
    (define-record Human
      [name : String #:contract string?]
      [age : Varint #:contract (integer-in 0 100)])
    (define h (make-Human #:name "Bogdan" #:age 30))
    (define bs (with-output-to-bytes (λ () (write-record Human h))))
    (check-equal? h (read-record Human (open-input-bytes bs)))))


;; enum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 define-enum
 enum-out)

(define-generics enum-variant-writer
  {write-enum-variant enum-variant-writer [out]})

(define (read-enum-variant info [in current-input-port])
  (define variants (enum-info-variants info))
  (define variant-idx (read-field UVarint in))
  (unless (>= (vector-length variants) variant-idx)
    (error 'read-enum-variant "unknown variant index ~a for enum ~a" variant-idx (enum-info-name info)))
  (define variant (vector-ref variants variant-idx))
  (apply
   (enum-variant-constructor variant)
   (for/list ([f (in-list (enum-variant-fields variant))])
     (read-field (enum-variant-field-type f) in))))

(define (do-write-enum-variant info idx v [out (current-output-port)])
  (define variant (vector-ref (enum-info-variants info) idx))
  (write-field UVarint (enum-variant-id variant) out)
  (define last-field-name #f)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define message
                       (format "~a~n  while writing enum variant field: ~a.~a.~a"
                               (exn-message e)
                               (enum-info-name info)
                               (enum-variant-name variant)
                               last-field-name))
                     (raise (exn:fail message (current-continuation-marks))))])
    (for ([f (in-list (enum-variant-fields variant))])
      (define type (enum-variant-field-type f))
      (define value ((enum-variant-field-accessor f) v))
      (set! last-field-name (enum-variant-field-name f))
      (write-field type value out))))

(provide
 enum-infos
 (struct-out enum-info)
 (struct-out enum-variant)
 (struct-out enum-variant-field))

(struct enum-variant-field (name type accessor))
(struct enum-variant (id name constructor fields))
(struct enum-info ([id #:mutable] name protocols variants))

(define enum-infos (make-hasheqv))
(define enum-info-sequencer
  (make-sequencer
   enum-infos
   enum-info-name
   set-enum-info-id!))

(define-syntax (define-enum stx)
  (define-syntax-class enum-variant
    #:literals (:)
    (pattern [name:id {fld-name:id : fld-type:expr} ...]))

  (syntax-parse stx
    #:literals (:)
    [(_ {~or name:id (name:id : proto:protocol ...+)} variant:enum-variant ...+)
     #:with name? (format-id #'name "~a?" #'name)
     #:with (variant.idx ...)
     (for/list ([stx (in-list (syntax-e #'(variant ...)))]
                [idx (in-naturals)])
       (datum->syntax stx idx))
     #:with (variant.qualname ...)
     (for/list ([stx (in-list (syntax-e #'(variant.name ...)))])
       (format-id #'name "~a.~a" #'name stx))
     #:with (variant.qualname? ...)
     (for/list ([stx (in-list (syntax-e #'(variant.qualname ...)))])
       (format-id stx "~a?" stx))
     #:with ((variant.fld-accessor-id ...) ...)
     (for/list ([qual-stx (in-list (syntax-e #'(variant.qualname ...)))]
                [names-stx (in-list (syntax-e #'((variant.fld-name ...) ...)))])
       (for/list ([stx (in-list (syntax-e names-stx))])
         (format-id qual-stx "~a-~a" qual-stx stx)))
     #'(begin
         (define-syntax (name stx-or-mode)
           (case stx-or-mode
             [(provide-enum)
              #'(combine-out name name?
                             variant.qualname ...
                             variant.qualname? ...
                             variant.fld-accessor-id ... ...)]
             [else
              (unless (syntax? stx-or-mode)
                (raise-syntax-error 'name "unexpected argument to transformer" stx-or-mode))
              (syntax-case stx-or-mode ()
                [id (identifier? #'id) #'info])]))
         (struct root ()
           #:transparent
           #:reflection-name 'name)
         (define name? root?)
         (struct variant.qualname root (variant.fld-name ...)
           #:transparent
           #:methods gen:enum-variant-writer
           {(define (write-enum-variant self [out (current-output-port)])
              (do-write-enum-variant info variant.idx self out))}) ...
         (define protocols
           {~? (list proto.e ...) null})
         (define variants
           (vector
            (enum-variant
             variant.idx
             'variant.name
             variant.qualname
             (list
              (enum-variant-field
               'variant.fld-name
               (->field-type 'Enum variant.fld-type)
               variant.fld-accessor-id) ...)) ...))
         (define info
           (enum-info #f 'name protocols variants))
         (sequencer-add! enum-info-sequencer info))]))

(define-syntax enum-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ id)
        (define export-stx
          ((syntax-local-value #'id) 'provide-enum))
        (expand-export export-stx modes)]))))

(module+ test
  (test-case "enum serde"
    (define-enum Column
      [null]
      [default]
      [text {t : String}])

    (define tests
      (list (Column.null)
            (Column.default)
            (Column.text "hello")))
    (for ([t (in-list tests)])
      (define bs (with-output-to-bytes (λ () (write-enum-variant t))))
      (check-equal? t (read-enum-variant Column (open-input-bytes bs))))))


;; varint ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 read-uvarint
 write-uvarint)

(define-inline (write-uvarint* v out)
  ;; PERF: It is faster to batch up the bytes and issue a single
  ;; write to the output port than it is to make many individual
  ;; writes.
  (define bs
    (let loop ([bs null] [n v])
      (define-values (q r)
        (quotient/remainder n #x80))
      (if (zero? q)
          (apply bytes (reverse (cons r bs)))
          (loop (cons (bitwise-ior r #x80) bs) q))))
  (write-bytes bs out))

(define (write-uvarint v [out (current-output-port)])
  (if (< v #x80)
      (write-byte v out)
      (write-uvarint* v out)))

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
 HashTable
 Listof
 Optional
 Record
 Enum
 Delay
 StringConvertible

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

(define-syntax (define-float-field-type stx)
  (syntax-parse stx
    [(_ id:id size:number)
     #'(define-field-type id
         #:read (λ (in) (floating-point-bytes->real (read-bytes size in) #t))
         #:write (λ (n out) (write-bytes (real->floating-point-bytes n size #t) out)))]))

(define-float-field-type Float32 4)
(define-float-field-type Float64 8)

(define-syntax (define-integer-field-type stx)
  (syntax-parse stx
    [(_ id:id size:number)
     #'(define-integer-field-type id size #f)]
    [(_ id:id size:number #:signed)
     #'(define-integer-field-type id size #t)]
    [(_ id:id size:number signed?:expr)
     #'(define-field-type id
         #:read (λ (in) (integer-bytes->integer (read-bytes size in) signed? #t))
         #:write (λ (n out) (write-bytes (integer->integer-bytes n size signed? #t) out)))]))

(define-integer-field-type Int16 2 #:signed)
(define-integer-field-type Int32 4 #:signed)
(define-integer-field-type UInt16 2)
(define-integer-field-type UInt32 4)

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

(define-field-type Void
  #:read (λ (_in) (error 'Void "cannot read Void values"))
  #:write (λ (_v _out) (error 'Void "cannot write Void values"))
  #:swift (λ () "Void"))

(provide
 ->field-type)

(define (->field-type who t)
  (cond
    [(record-info? t)
     (Record t)]
    [(enum-info? t)
     (Enum t)]
    [else
     (begin0 t
       (unless (field-type? t)
         (raise-argument-error who "(or/c field-type? record-info?)" t)))]))

(define (HashTable k v)
  (let ([k (->field-type 'HashTable k)]
        [v (->field-type 'HashTable v)])
    (define k-read-proc (field-type-read-proc k))
    (define v-read-proc (field-type-read-proc v))
    (define k-write-proc (field-type-write-proc k))
    (define v-write-proc (field-type-write-proc v))
    (define k-swift-type ((field-type-swift-proc k)))
    (define v-swift-type ((field-type-swift-proc v)))
    (field-type
     (λ (in)
       (for/hash ([_ (in-range (read-varint in))])
         (values
          (k-read-proc in)
          (v-read-proc in))))
     (λ (h out)
       (write-varint (hash-count h) out)
       (for ([(k v) (in-hash h)])
         (k-write-proc k out)
         (v-write-proc v out)))
     (λ ()
       (format "[~a: ~a]"
               k-swift-type
               v-swift-type)))))

(define (Listof t)
  (let ([t (->field-type 'Listof t)])
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

(define (Optional t)
  (let ([t (->field-type 'Optional t)])
    (define read-proc (field-type-read-proc t))
    (define write-proc (field-type-write-proc t))
    (define swift-type ((field-type-swift-proc t)))
    (field-type
     (λ (in)
       (and (not (zero? (read-byte in)))
            (read-proc in)))
     (λ (v out)
       (cond
         [v
          (write-byte 1 out)
          (write-proc v out)]
         [else
          (write-byte 0 out)]))
     (λ ()
       (format "~a?" swift-type)))))

(define (Record t)
  (unless (record-info? t)
    (raise-argument-error 'Record "record-info?" t))
  (field-type
   (λ (in) (read-record t in))
   (λ (v out) (write-record t v out))
   (λ () (symbol->string (record-info-name t)))))

(define (Enum t)
  (unless (enum-info? t)
    (raise-argument-error 'Enum "enum-info?" t))
  (field-type
   (λ (in) (read-enum-variant t in))
   (λ (v out) (write-enum-variant v out))
   (λ () (symbol->string (enum-info-name t)))))

(define (Delay* t-proc)
  (define (force)
    (->field-type 'Delay (t-proc)))
  (field-type
   (λ (in) ((field-type-read-proc (force)) in))
   (λ (v out) ((field-type-write-proc (force)) v out))
   (λ () ((field-type-swift-proc (force))))))

(define-syntax-rule (Delay e)
  (Delay* (λ () e)))

(define (StringConvertible string-> ->string)
  (field-type
   (λ (in) (string-> ((field-type-read-proc String) in)))
   (λ (v out) ((field-type-write-proc String) (->string v) out))
   (λ () ((field-type-swift-proc String)))))

(module+ test
  (require racket/string)

  (test-case "complex field serde"
    (define-record Example
      [b : Bool #:contract boolean?]
      [i : Varint #:contract integer?]
      [s : String #:contract string?]
      [l : (Listof Varint) #:contract list?])
    (define v (Example #t -1 "hello" '(0 1 2 #x-FF #x7F #xFFFF)))
    (define bs (with-output-to-bytes (λ () (write-record Example v))))
    (check-equal? v (read-record Example (open-input-bytes bs))))

  (test-case "container serde"
    (define-record Story
      [id : UVarint #:contract exact-integer?]
      [title : String #:contract string?]
      [comments : (Listof UVarint) #:contract (listof exact-integer?)]
      [metadata : (HashTable Symbol String) #:contract (hash/c symbol? string?)])
    (define-record Stories
      [stories : (Listof Story) #:contract (listof Story?)])
    (define v
      (Stories
       (list (Story 0 "a" null (hash 'a "a" 'b "b"))
             (Story 1 "b" '(1 2 3) (hash 'c "def")))))
    (define bs (with-output-to-bytes (λ () (write-record Stories v))))
    (check-equal? v (read-record Stories (open-input-bytes bs))))

  (test-case "nested record serde"
    (define-record A
      [s : String])
    (define-record B
      [a : A])
    (define v
      (B (A "test")))
    (define bs (with-output-to-bytes (λ () (write-record B v))))
    (check-equal? v (read-record B (open-input-bytes bs))))

  (test-case "record with enum serde"
    (define-enum Result
      [err {message : String}]
      [ok {value : UVarint}])
    (define-record C
      [res : Result])
    (define v
      (C (Result.err "an error")))
    (define bs (with-output-to-bytes (λ () (write-record C v))))
    (check-equal? v (read-record C (open-input-bytes bs))))

  (test-case "mutually-recursive types"
    (define-enum ApplyResult
      [stack {s : (Delay Stack)}]
      [text {t : String}])
    (define-record Stack
      [direction : Symbol]
      [children : (Listof ApplyResult)])
    (define v
      (ApplyResult.stack
       (make-Stack
        #:direction 'horizontal
        #:children (list
                    (ApplyResult.text "Hello")
                    (ApplyResult.stack
                     (make-Stack
                      #:direction 'vertical
                      #:children (list (ApplyResult.text "a")
                                       (ApplyResult.text "b"))))))))
    (define bs (with-output-to-bytes (λ () (write-field (Enum ApplyResult) v))))
    (check-equal? v (read-field (Enum ApplyResult) (open-input-bytes bs)))
    (check-exn
     (regexp
      (regexp-quote
       (string-join
        '("string->bytes/utf-8: contract violation"
          "  expected: string?"
          "  given: #f"
          "  while writing enum variant field: ApplyResult.text.t"
          "  while writing record field: Stack.children")
        "\n")))
     (lambda ()
       (write-record
        Stack
        (Stack
         'horizontal
         (list
          (ApplyResult.text #f)))
        (open-output-nowhere)))))

  (test-case "enum with protocols"
    (define-enum (Foo : Equatable)
      [foo]
      [bar])
    (check-equal?
     (enum-info-protocols Foo)
     '(Equatable)))

  (test-case "record with protocols"
    (define-record (Foo : Equatable Identifiable)
      [id : UVarint])
    (check-equal?
     (record-info-protocols Foo)
     '(Equatable Identifiable))))
