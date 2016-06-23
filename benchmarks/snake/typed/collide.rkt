#lang typed/racket

(require "data.rkt"
         "const.rkt")

;; Is the snake colliding with any of the walls?
(: snake-wall-collide? : (Snake . -> . Boolean))
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

(: head-collide? : (Posn . -> . Boolean))
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(: snake-self-collide? : (Snake . -> . Boolean))
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

(: segs-self-collide? : (Posn (Listof Posn) . -> . Boolean))
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))

(provide
 (contract-out
  [snake-wall-collide? (SNAKE/C . ->/c . boolean?)]
  [snake-self-collide? (SNAKE/C . ->/c . boolean?)]))
