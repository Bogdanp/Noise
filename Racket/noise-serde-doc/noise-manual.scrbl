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


@section[#:tag "serde"]{Serialization & Deserialization}
@defmodule[noise/serde]

A @deftech{record} is a data structure that is shared between Swift
and Racket.  In both languages, they are represented by structs.  Use
the @tt{raco} command @tt{noise-serde-codegen} to generate Swift
definitions for records reachable from a given root module.

@defproc[(record? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{record}.
}

@defproc[(record-info? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a value containing runtime
  information about a @tech{record}.
}

@defproc[(read-record [in input-port? (current-input-port)]) any/c]{
  Reads a @tech{record} from @racket[in].
}

@defproc[(write-record [r any/c]
                       [out output-port? (current-output-port)]) void?]{
  Writes @racket[r] to @racket[out].  Raises a contract error if
  @racket[r] is not an instance of a @tech{record}.
}

@defform[(define-record name
           field ...)
         #:grammar ([field [field-id field-type]
                           [field-id field-type field-ctc-expr]
                           [(field-id default-expr) field-type]
                           [(field-id default-expr) field-type field-ctc-expr]])
         #:contracts ([field-type field-type?])]{

  Defines a record called @racket[name] with the given set of
  @racket[field]s.  Records are backed by structs and generate smart
  constructors that take a keyword argument for every field.  Smart
  constructors are named by prepending @tt{make-} to the record name
  and bound at phase level 0.

  Opaque information about each record type is bound at phase level 0
  to a variable named by prepending @tt{record:} to the record name.
  Every record gets assigned a globally-unique identifier.  The GUIDs
  are issued in definition order.

  @examples[
    (require noise/serde
             racket/contract)
    (define-record Human
     [name String string?]
     [age UVarint (integer-in 0 125)])
    (make-Human
     #:name "Bogdan"
     #:age 30)
    record:Human
  ]
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
  @defthing[String field-type?]
  @defthing[Symbol field-type?]
  @defthing[UVarint field-type?]
  @defthing[Varint field-type?]
)]{

  @tech{Field types} for primitive values.

  The @racket[UVarint] and @racket[Varint] field types serialize
  unsigned and signed integer values, respectively, using a
  variable-length encoding.  In Swift, these values are represented by
  @tt{UInt64} and @tt{Int64}, respectively.
}

@defproc[(Listof [t field-type?]) field-type?]{
  A constructor for @tech{field types} that represent a list of values
  of type @racket[t].  In Swift, these values are represented by
  arrays of the subtype.
}

@defthing[Record field-type?]{
  A @tech{field type} that serializes record values by tagging them
  with their globally-unique id.  In Swift, these values are
  represented by the @tt{Record} enum.
}

@defproc[(Untagged [ri record-info?]) field-type?]{
  A constructor for @tech{field types} that serialize records without
  tagging.  Useful for creating homogeneous lists of records and for
  embedding records directly into one another.
}


@section[#:tag "backends"]{Backends}
@defmodule[noise/backend]

@defproc[(serve [in-fd exact-integer?]
                [out-fd exact-integer?]
                [handler (-> any/c any/c)]) (-> void?)]{

  Converts the file descriptors represented by @racket[in-fd] and
  @racket[out-fd] to an input port and an output port, respectively,
  then spawns a thread that reads requests from the input port in the
  form of @tech{records}.  Calls @racket[handler] with every request
  and writes the result value to the output port.  Handlers are run in
  their own threads and multiple requests may be handled concurrently.

  Returns a procedure that stops the server when called.
}
