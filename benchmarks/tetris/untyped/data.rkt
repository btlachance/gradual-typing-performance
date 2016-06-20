#lang racket

(struct posn (x y))
(struct block (x y color))
(struct tetra (center blocks))
(struct world (tetra blocks))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define COLOR/C symbol?)
(define POSN/C posn?)
(define BLOCK/C block?)
(define BSET/C (listof BLOCK/C))  
(define TETRA/C tetra?)
(define WORLD/C world?)

(provide
(contract-out
  [block (-> real? real? COLOR/C block?)]
  [block-x (-> block? real?)]
  [block-y (-> block? real?)]
  [block-color (-> block? COLOR/C)]

  [posn (-> real? real? posn?)]
  [posn-x (-> posn? real?)]
  [posn-y (-> posn? real?)]

  [tetra (-> POSN/C BSET/C tetra?)]
  [tetra-center (-> tetra? POSN/C)]
  [tetra-blocks (-> tetra? BSET/C)]
  
  [world (-> TETRA/C BSET/C world?)]
  [world-tetra (-> world? TETRA/C)]
  [world-blocks (-> world? BSET/C)]

  [posn=? (POSN/C POSN/C . -> . boolean?)])
 COLOR/C
 POSN/C
 BLOCK/C
 TETRA/C
 WORLD/C
 BSET/C)
