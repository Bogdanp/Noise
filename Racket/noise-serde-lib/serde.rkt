#lang racket/base

(require "private/serde.rkt")
(provide
 record?
 record-info?
 define-record
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
