#lang typed/racket

(: block-size Integer)
(define block-size 20)

(: board-height Integer)
(define board-height 20)

(: board-width Integer)
(define board-width 10)

(provide
 (contract-out
  [block-size integer?]
  [board-width integer?]
  [board-height integer?]))
