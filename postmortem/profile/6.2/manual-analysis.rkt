#lang racket/base

(require math/statistics racket/format)

(define (rnd n)
  (~r n #:precision '(= 2)))

;Average contract runtime, over 10 runs of the profiler.
;
;|------------+----------+------|
;| PROJECT    | MEAN (%) |  STD |
;|------------+----------+------|
;| gregor     |    83.29 | 4.01 |
;| kcfa       |    91.38 | 0.25 |
;| lnm        |    81.19 | 0.73 |
;| mbta       |    39.02 | 3.64 |
;| morse-code |    29.53 | 6.81 |
;| quad       |    80.42 | 0.96 |
;| snake      |    98.28 | 0.21 |
;| suffixtree |    93.53 | 0.18 |
;| synth      |    82.70 | 1.22 |
;| tetris     |    95.67 | 0.35 |
;| zordoz     |    94.59 | 0.10 |
;|------------+----------+------|

(define gregor
'((B 2093 1156 1156 1176 1140 1160 2204 1190 1170 1157)
 (P 1001/2 260 379 617/2 233 717/2 1177/2 155 460 230)
 (H )
 (L (57/2 37 28)
    (10 17/2)
    (10 20 33/2 10)
    (20 10 10 10)
    (63/2 10)
    (10 10)
    (47/2 20 103/2 20)
    (27/2 10)
    (38 12 25)
    (20 30 8))))

(define kcfa
'((B 461673 461893 481037 529180 481256 477530 477670 476194 477997 481686)
  (P 130567 132598 137351 146878 137730 135250 132345 137085 135447 273331/2)
  (H)
  (L)))

(define lnm
'(
  (B 54000 53607 56614 56820 62497 52790 53407 52440 52217 52873)
  (P)
  (H (8413/2)
     (3878)
     (8789/2)
     (4113)
     (4013 23/2)
     (7993/2)
     (8703/2)
     (3914)
     (4162)
     (8867/2))
  (L (8413/2 10 78241/2)
     (78279/2 3878 10 10)
     (83099/2 8789/2)
     (42625 4113)
     (46013 4013 23/2)
     (76163/2 7993/2 23/2)
     (38377 8703/2 12)
     (76463/2 3914 17/2)
     (76285/2 4162 10)
     (37772 8867/2 15 10))))

(define mbta
'((B 1590 1626 1617 1600 1606 1617 1586 1586 1746 2000 )
  (P)
  (H)
  (L (581/2 367/2)
     (292 150)
     (208 355/2)
     (228 379/2)
     (250 199)
     (537/2 195)
     (202 197/2)
     (373/2 179)
     (577/2 291/2)
     (247 219))))

(define morse-code
'((B 530 497 540 497 497 933 624 517 783 497)
  (P)
  (H)
  (L)))

(define quad
'(
  (B 12240 10720 10133 10137 11180 10650 11087 10263 11590 10650)
  (P 10 12 10 43/2 17/2 10)
  (H (30 20 23/2)
     (25 10)
     (20 10 10)
     (42 15 10 23/2 10)
     (20 20 53/2 20 10 10)
     (30 20 20 10)
     (30 20 10 10 10)
     (45 30 10))
  (L)))

(define sieve
'((B 1827040 1891320 1628400 1619530)
  (P 402080 642983/2  1095053/2 538279/2)
  (H 1616817/2 1503035/2 1610221/2 721519)
  (L)))

(define snake
'((B 37933 36837 36987 59667 36364 35513 35270 35300 34830 35077)
  (P 18626 17323 35299/2 55829/2 35061/2 16864 33815/2 34817/2 17139 33697/2)
  (H)
  (L)))

(define suffixtree
'(
  (B 292063 303937 303060 299440 296360 305313 307150 309063 300370 305733)
  (P 96557/2 50733 49543 98195/2 97009/2 50431 50419 99815/2 48280 101869/2)
  (H 592 787 671 597 989/2 1177/2 720 759 1411/2 659)
  (L)))

(define synth
'((B 17033 14867 11470 11450 11603 11677 11844 11526 11463 11480)
  (P)
  (H 14077/2 11533/2 9499/2 9415/2 4482 9135/2 4535 4594 9603/2 4630)
  (L)))

(define tetris
'((B 54477 56984 55544 55287 55940 53967 54327 53973 54834 54500)
  (P 45233/2 23942 23996 45871/2 47903/2 23238 22640 22838 46937/2 46267/2)
  (H)
  (L)))

(define zordoz
'((B 479070 484310 483377 477380 486550 464933 474724 470260 472970 467964)
  (P 193655 390503/2 395779/2 395661/2 199348 191646 193949 386459/2 192409 380069/2)
  (H)
  (L 201307 202788 410751/2 205207 206971 199541 402729/2 400233/2 199687 394737/2)))

(define (check-tags dat n)
  (for ([v* (in-list dat)]
        [tag (in-list '(B P H L))])
    (unless (eq? (car v*) tag)
      (error 'check-tags (format "Failure, tag ~a on index ~a" tag n)))))

(define (v*->str v* full*)
  (cond [(not (null? v*))
         (define p* (for/list ([v (in-list v*)] [f (in-list full*)])
                      (* 100 (if (list? v)
                          (/ (apply + v) f)
                          (/ v f)))))
         (define m  (mean p*))
         (define s  (stddev/mean m p*))
         (format "~a (~a)" (rnd m) (rnd s))]
        [else
         ""]))

(module+ main
  (for ([n (in-naturals)]
        [dat (in-list (list
                      gregor
                      kcfa
                      lnm
                      mbta
                      morse-code
                      quad
                      sieve
                      snake
                      suffixtree
                      synth
                      tetris
                      zordoz))])
    (check-tags dat n)
    (define full-time* (cdr (car dat)))
    (define pred-time* (cdr (cadr dat)))
    (define high-time* (cdr (caddr dat)))
    (define libr-time* (cdr (cadddr dat)))
    (printf " ~a | ~a | ~a |\n"
            (v*->str pred-time* full-time*)
            (v*->str high-time* full-time*)
            (v*->str libr-time* full-time*))))