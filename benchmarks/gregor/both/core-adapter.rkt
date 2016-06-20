#lang typed/racket/base

;; Adapter for gregor's `structs.rkt` file.

(provide
  (struct-out YMD)
  (struct-out HMSN)
  Month month?
  nonneg?)

(require "../base/types.rkt")

(require "core-structs.rkt")
(: nonneg? (FlatCon Real Nonnegative-Real))
(define (nonneg? [x : Real])
  (>= x 0))
