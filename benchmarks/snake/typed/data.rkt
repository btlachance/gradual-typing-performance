#lang typed/racket

(struct: snake ([dir  : Dir]
                [segs : (NEListof Posn)]))
(struct: world ([snake : Snake]
                [food  : Posn]))

(struct: posn ([x : Real]
               [y : Real]))

(define-type Posn  posn)
(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Snake snake)
(define-type Dir (U "up" "down" "left" "right"))
(define-type World world)

(: nelistof (All (a b) (-> (Con a b) (Con Any (Pairof b (Listof b))))))
(define (nelistof c) (cons/c c (listof c)))
(define DIR/C (make-predicate Dir))
(define POSN/C posn?)
(define SNAKE/C snake?)
(define WORLD/C world?)

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(provide
 Posn
 Snake
 NEListof
 World
 Dir)
(provide
 (contract-out
  [posn (->/c real? real? POSN/C)]
  [posn? (->/c any/c boolean?)]
  [posn-x (->/c POSN/C real?)]
  [posn-y (->/c POSN/C real?)]
  [posn=? (POSN/C POSN/C . ->/c . boolean?)]
  [snake (->/c DIR/C (nelistof POSN/C) SNAKE/C)]
  [snake? (->/c any/c boolean?)]
  [snake-dir (->/c SNAKE/C DIR/C)]
  [snake-segs (->/c SNAKE/C (nelistof POSN/C))]
  [world (->/c SNAKE/C POSN/C WORLD/C)]
  [world? (->/c any/c boolean?)]
  [world-snake (->/c WORLD/C SNAKE/C)]
  [world-food (->/c WORLD/C POSN/C)]
  [DIR/C any/c]
  [POSN/C any/c]
  [SNAKE/C any/c]
  [WORLD/C any/c]
  [nelistof any/c]))
