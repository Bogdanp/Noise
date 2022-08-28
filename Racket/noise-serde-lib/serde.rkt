#lang racket/base

(require "private/serde.rkt")
(provide
 define-record
 record-out
 record?
 record-info?
 read-record
 write-record

 field-type?
 Bool
 Bytes
 Float32
 Float64
 Int16
 Int32
 Varint
 UInt16
 UInt32
 UVarint
 Listof
 Optional
 Record
 String
 Symbol
 Untagged)
