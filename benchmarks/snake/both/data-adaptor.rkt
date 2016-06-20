#lang typed/racket

(require benchmark-util)

(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct snake ([dir : Dir]
                   [segs : (NEListof Posn)])]
  [#:struct world ([snake : Snake]
                   [food  : Posn])])

(provide
 (struct-out posn)
 (struct-out snake)
 (struct-out world)
 Dir
 Snake
 World
 Posn
 NEListof
 posn=?
 DIR/C
 POSN/C
 SNAKE/C
 WORLD/C)
