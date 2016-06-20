#lang typed/racket/base

(require/typed/provide "../base/tzinfo/main.rkt"
  [system-tzid (-> (U tz #f))]
  [#:struct tzgap ([starts-at : Natural]
                   [offset-before : tzoffset]
                   [offset-after : tzoffset])]
  [#:struct tzoffset ([utc-seconds : Integer]
                      [dst? : Boolean]
                      [abbreviation : String])]
  [#:struct tzoverlap ([offset-before : tzoffset]
                       [offset-after : tzoffset])]
  [local-seconds->tzoffset (-> String Integer (U tzoffset tzgap tzoverlap))]
  [utc-seconds->tzoffset (-> String Exact-Rational tzoffset)]
  )

(define tz/c (or/c string?
                   (integer-in -64800 64800)))

(provide tz/c)

(provide tz)
(define-type tz (U String Integer))
