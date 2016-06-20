#lang typed/racket/base

;; Adapter for gregor's `structs.rkt` file.

(provide
  (struct-out YMD)
  (struct-out HMSN)
  Month)

(require "structs.rkt"
         "../base/types.rkt")

