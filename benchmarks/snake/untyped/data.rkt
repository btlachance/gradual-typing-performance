#lang racket

(struct snake (dir segs))
(struct world (snake food))
(struct posn (x y))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(define (nelistof c) (cons/c c (listof c)))
(define DIR/C (or/c "up" "down" "left" "right"))
(define POSN/C posn?)
(define SNAKE/C snake?)
(define WORLD/C world?)

(provide
 (contract-out
  [posn (-> real? real? POSN/C)]
  [posn? (-> any/c boolean?)]
  [posn-x (-> POSN/C real?)]
  [posn-y (-> POSN/C real?)]
  [posn=? (POSN/C POSN/C . -> . boolean?)]
  [snake (-> DIR/C (nelistof POSN/C) SNAKE/C)]
  [snake? (-> any/c boolean?)]
  [snake-dir (-> SNAKE/C DIR/C)]
  [snake-segs (-> SNAKE/C (nelistof POSN/C))]
  [world (-> SNAKE/C POSN/C WORLD/C)]
  [world? (-> any/c boolean?)]
  [world-snake (-> WORLD/C SNAKE/C)]
  [world-food (-> WORLD/C POSN/C)]
  [DIR/C any/c]
  [POSN/C any/c]
  [SNAKE/C any/c]
  [WORLD/C any/c]
  [nelistof any/c]))
