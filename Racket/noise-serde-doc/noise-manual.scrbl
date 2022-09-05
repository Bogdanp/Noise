#lang scribble/manual

@(require scribble/example
          (for-label noise/backend
                     noise/serde
                     racket/base
                     racket/contract))

@title{Noise Ser/de}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define (noise-anchor . content)
  (apply link "https://github.com/Bogdanp/Noise" content))

This is a companion package to @noise-anchor{Noise} that provides
utilities for serializing and deserializing data structures between
Swift and Racket.  Backwards-compatibility is not guaranteed.  This
package will not be published to the package index and it should be
installed from a cloned version of Noise.

@(define ev
   (let ([ev (make-base-eval)])
     (begin0 ev
       (ev '(require noise/backend noise/serde racket/contract)))))

@section[#:tag "serde"]{Serialization & Deserialization}
@defmodule[noise/serde]

@subsection{Records}

A @deftech{record} is a data structure that is shared between Swift
and Racket.  In both languages, they are represented by structs.  Use
the @tt{raco} command @tt{noise-serde-codegen} to generate Swift
definitions for records reachable from a given root module.

@defform[(define-record name
           field ...)
         #:grammar ([field [field-id field-type]
                           [field-id field-type field-ctc-expr]
                           [(field-id default-expr) field-type]
                           [(field-id default-expr) field-type field-ctc-expr]])
         #:contracts ([field-type (or/c field-type? enum-info? record-info?)])]{

  Defines a record called @racket[name] with the given set of
  @racket[field]s.  Records are backed by structs and generate smart
  constructors that take a keyword argument for every field.  Smart
  constructors are named by prepending @tt{make-} to the record name
  and bound at phase level 0.

  Record @racket[name]s must be unique across all modules.

  @examples[
    #:eval ev
    (define-record Human
     [name String string?]
     [age UVarint (integer-in 0 125)])
    (make-Human
     #:name "Bogdan"
     #:age 30)
    Human
  ]
}

@defform[(record-out id)]{
  Exports the bindings associated with a record @racket[id].
}

@defproc[(record-info? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a value containing runtime
  information about a @tech{record}.
}


@subsection{Enumerations}

An @deftech{enumeration} is a tagged union of product types.  In
Racket, enum variants are represented by individual structs that
inherit from a common base.  In Swift, they are represented using
regular enums.

@deftogether[(
  @defidform[:]
  @defform[
    #:literals (:)
    (define-enum name
      [variant-name field ...] ...+)
    #:grammar ([field {field-id : field-type}])
    #:contracts ([field-type (or/c field-type? enum-info? record-info?)])]
)]{

  Defines an enumeration called @racket[name] with the given set of
  variants.  Enumeration @racket[name]s must be unique across all
  modules.

  @examples[
    #:eval ev
    (define-enum Result
      [ok]
      [err {message : String}])
    (Result? (Result.ok))
    (Result? (Result.err "example"))
    Result
  ]
}

@defform[(enum-out id)]{
  Exports the bindings associated with an enum @racket[id].
}

@defproc[(enum-info? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a value containing runtime
  information about an @tech{enumeration}.
}


@subsection{Field Types}

@deftech{Field types} control how individual values are serialized and
deserialized.

@defproc[(field-type? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{field type}.
}

@deftogether[(
  @defthing[Bool field-type?]
  @defthing[Bytes field-type?]
  @defthing[Float32 field-type?]
  @defthing[Float64 field-type?]
  @defthing[Int16 field-type?]
  @defthing[Int32 field-type?]
  @defthing[Varint field-type?]
  @defthing[UInt16 field-type?]
  @defthing[UInt32 field-type?]
  @defthing[UVarint field-type?]
  @defthing[String field-type?]
  @defthing[Symbol field-type?]
)]{

  @tech{Field types} for primitive values.

  The @racket[Varint] and @racket[UVarint] field types serialize
  signed and unsigned integer values, respectively, using a
  variable-length encoding.  In Swift, these values are represented by
  @tt{Int64} and @tt{UInt64}.
}

@defproc[(Listof [t (or/c field-type? enum-info? record-info?)]) field-type?]{
  A constructor for @tech{field types} that represent lists composed
  of @racket[field-type] values.  In Swift, these values are
  represented by arrays of the subtype.
}

@defproc[(Optional [t (or/c field-type? enum-info? record-info?)]) field-type?]{
  A constructor for optional @tech{field types}.
}


@section[#:tag "backends"]{Backends}
@defmodule[noise/backend]

The @racketmodname[noise/backend] module has an internal
@deftech{handler registry} that is used to map remote procedure call
ids to handler procedures.

@defform[
  #:literals (:)
  (define-rpc (id arg ... : response-type-expr)
    body ...+)
  #:grammar [(arg [arg-label arg-id : arg-type-expr])]
  #:contracts ([arg-type-expr (or/c field-type? enum-info? record-info?)]
               [response-type-expr (or/c field-type? enum-info? record-info?)])
]{
  Defines a procedure named @racket[id] and registers an RPC handler
  for it in the @tech{handler registry}.

  The @tt{noise-serde-codegen} command automatically generates Swift
  code to handle calling these procedures.  In Swift, the RPC
  @racket[id], @racket[arg-label]s and @racket[arg-id]s are converted
  to camel case.  The @racket[arg-label]s have no meaning in Racket.

  RPC @racket[id]s must be unique across all modules.

  @examples[
    #:eval ev
    (define-rpc (get-human-name [of h : Human] : String)
      (Human-name h))
    (get-human-name (make-Human #:name "Bogdan" #:age 30))
  ]
}

@defproc[(serve [in-fd exact-integer?]
                [out-fd exact-integer?]) (-> void?)]{

  Converts the file descriptors represented by @racket[in-fd] and
  @racket[out-fd] to an input port and an output port, respectively,
  then spawns a thread that reads requests from the input port in the
  form of @tech{records}.  Request handlers are defined using
  @racket[define-rpc].  Handlers are run in their own threads and
  multiple requests may be handled concurrently.

  Returns a procedure that stops the server when applied.
}
