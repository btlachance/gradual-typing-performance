#lang typed/racket
;; Movie handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "data.rkt"
         "collide.rkt"
         "motion.rkt")

(: handle-key : (World String . -> . World) )
(define (handle-key w ke)
  (cond [(equal? ke "w") (world-change-dir w "up")]
        [(equal? ke "s") (world-change-dir w "down")]
        [(equal? ke "a") (world-change-dir w "left")]
        [(equal? ke "d") (world-change-dir w "right")]
        [else w]))

(: game-over? : (World . -> . Boolean))
(define (game-over? w)
  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))

(provide
 (contract-out
  [handle-key (WORLD/C string? . ->/c . WORLD/C)]
  [game-over? (WORLD/C . ->/c . boolean?)]))
