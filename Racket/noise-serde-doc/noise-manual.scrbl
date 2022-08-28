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
         #:contracts ([field-type (or/c field-type? record-info?)])]{

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

@defproc[(record? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an instance of a @tech{record}.
}

@defproc[(read-record [in input-port? (current-input-port)]) any/c]{
  Reads a @tech{record} from @racket[in].
}

@defproc[(write-record [r any/c]
                       [out output-port? (current-output-port)]) void?]{
  Writes @racket[r] to @racket[out].  Raises a contract error if
  @racket[r] is not an instance of a @tech{record}.
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

@defproc[(Listof [t (or/c field-type? record-info?)]) field-type?]{
  A constructor for @tech{field types} that represent lists composed
  of @racket[field-type] values.  In Swift, these values are
  represented by arrays of the subtype.  When @racket[t] is a
  @racket[record-info?] value, it is converted to an @racket[Untagged]
  field.
}

@defproc[(Optional [t (or/c field-type? record-info?)]) field-type?]{
  A constructor for optional @tech{field types}.  When @racket[t] is a
  @racket[record-info?] value , it is converted to an
  @racket[Untagged] field.
}

@defthing[Record field-type?]{
  A @tech{field type} that serializes record values by tagging them
  with their unique id.  In Swift, these values are represented by the
  @tt{Record} enum.
}

@defproc[(Untagged [ri record-info?]) field-type?]{
  A constructor for @tech{field types} that serialize records without
  tagging.  Useful for creating homogeneous lists of records and for
  embedding records directly into one another.
}


@section[#:tag "backends"]{Backends}
@defmodule[noise/backend]

The @racketmodname[noise/backend] module has an internal
@deftech{handler registry} that is used to map remote procedure call
ids to handler procedures.

@deftogether[(
  @defidform[:]
  @defform[
    #:literals (:)
    (define-rpc (id arg ... : response-type-expr)
      body ...+)
    #:grammar [(arg [arg-label arg-id : arg-type-expr])]
    #:contracts ([arg-type-expr (or/c field-type? record-info?)]
                 [response-type-expr (or/c field-type? record-info?)])
  ]
)]{
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
