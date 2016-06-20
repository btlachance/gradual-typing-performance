#lang typed/racket

(struct: posn ([x : Real]
               [y : Real]))
(struct: block ([x : Real]
                [y : Real]
                [color : Symbol]))
(struct: tetra ([center : posn]
                [blocks : (Listof block)]))
(struct: world ([tetra : tetra]
                [blocks : (Listof block)]))

(define-type Color Symbol)
(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

(define COLOR/C symbol?)
(define POSN/C posn?)
(define BLOCK/C block?)
(define BSET/C (listof BLOCK/C))  
(define TETRA/C tetra?)
(define WORLD/C world?)

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide
 (contract-out
  [block (->/c real? real? COLOR/C block?)]
  [block-x (->/c block? real?)]
  [block-y (->/c block? real?)]
  [block-color (->/c block? COLOR/C)]
  [posn (->/c real? real? posn?)]
  [posn-x (->/c posn? real?)]
  [posn-y (->/c posn? real?)]
  [tetra (->/c POSN/C BSET/C tetra?)]
  [tetra-center (->/c tetra? POSN/C)]
  [tetra-blocks (->/c tetra? BSET/C)]
  [world (->/c TETRA/C BSET/C world?)]
  [world-tetra (->/c world? TETRA/C)]
  [world-blocks (->/c world? BSET/C)]
  [posn=? (POSN/C POSN/C . ->/c . boolean?)])
 COLOR/C
 Color
 POSN/C
 Posn
 BLOCK/C
 Block
 TETRA/C
 Tetra
 WORLD/C
 World
 BSET/C
 BSet)
