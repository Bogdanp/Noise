#lang racket/base

(require "private/serde.rkt")
(provide
 record?
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
 UVarint
 Varint)
