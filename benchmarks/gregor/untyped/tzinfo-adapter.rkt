#lang racket/base

(require racket/contract/base
         "../base/tzinfo/main.rkt")
(provide
 system-tzid
 (struct-out tzgap)
 (struct-out tzoffset)
 (struct-out tzoverlap)
 local-seconds->tzoffset
 utc-seconds->tzoffset)

(define tz/c (or/c string?
                   (integer-in -64800 64800)))

(provide tz/c)
