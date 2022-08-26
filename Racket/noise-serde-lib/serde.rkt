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
 Listof
 Record
 String
 Symbol
 Untagged
 UVarint
 Varint)
