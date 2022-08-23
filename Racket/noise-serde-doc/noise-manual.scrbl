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


@section{Serialization & Deserialization}
@defmodule[noise/serde]

A @deftech{record} is a data structure that is shared between Swift
and Racket.  In both languages, they are represented by structs.

Use the @tt{raco} command @tt{noise-serde-codegen} to generate Swift
definitions for records reachable from a given root module.

@defproc[(record? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{record}.
}

@defproc[(field-type? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a field type.
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
         #:contracts ([field-type (or/c field-type? record?)])]{

  Defines a record called @racket[name] with the given set of
  @racket[field]s.  Records are backed by structs and generate smart
  constructors that take a keyword argument for every field.  The
  smart constructors are named by prepending @tt{make-} to the record
  name.

  @examples[
    (require noise/serde
             racket/contract)
    (define-record Human
     [name String string?]
     [age UVarint (integer-in 0 125)])
    (make-Human
     #:name "Bogdan"
     #:age 30)
  ]
}

@section{Backends}
@defmodule[noise/backend]
