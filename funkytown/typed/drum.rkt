#lang typed/racket/base

(require (only-in "array-struct.rkt"
                  array-size
                  make-array
                  build-array
                  unsafe-vector->array)
         (only-in "array-utils.rkt"
                  array-shape-size
                  check-array-shape)
         (only-in "array-transform.rkt" array-append*)
         (only-in "synth.rkt" fs seconds->samples)
         "array-types.rkt")

(provide drum)

(: random-sample (-> Float))
(define (random-sample) (- (* 2.0 (random)) 1.0))

;; Drum "samples" (Arrays of floats)
;; TODO compute those at compile-time
(: bass-drum (Array Float))
(define bass-drum
  (let ()
    ;; 0.05 seconds of noise whose value changes every 12 samples
    (: n-samples Index)
    (define n-samples           (seconds->samples 0.05))
    (: n-different-samples Integer)
    (define n-different-samples (quotient n-samples 12))
    (: ds* In-Indexes)
    (define ds* (vector n-samples))
    (: ds  Indexes)
    (define ds
      (check-array-shape ds*
                         (λ () (raise-argument-error 'name "Indexes" ds))))
    (: vs (Vectorof Flonum))
    (define vs
      (for/vector : (Vectorof Flonum)
                  #:length (array-shape-size ds)
                  #:fill 0.0
                  ([i      : Natural (in-range n-different-samples)]
                   [sample : Flonum  (in-producer random-sample)]
                   #:when #t
                   [j      : Natural (in-range 12)])
                  sample))
    (unsafe-vector->array ds vs)))

(: snare (Array Float))
(define snare
  ;; 0.05 seconds of noise
  (let: ([indexes : In-Indexes
                  (vector (seconds->samples 0.05))]
         [arr-gen : (-> Indexes Flonum)
                  (lambda ([x : Indexes]) (random-sample))])
    (build-array indexes arr-gen)))

;; limited drum machine
(: drum (-> Natural Pattern Natural (Array Float)))
(define (drum n pattern tempo)
  (: samples-per-beat Natural)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (: make-drum (-> (Array Float) Natural (Array Float)))
  (define (make-drum drum-sample samples-per-beat)
    (array-append*
     (list drum-sample
           (make-array (vector (- samples-per-beat
                                  (array-size drum-sample)))
                       0.0))))
  (: O (Array Float))
  (define O     (make-drum bass-drum samples-per-beat))
  (: X (Array Float))
  (define X     (make-drum snare     samples-per-beat))
  (: pause (Array Float))
  (define pause (make-array (vector samples-per-beat) 0.0))
  (array-append*
   (for*/list : (Listof (Array Float)) ([i : Integer (in-range n)]
                                        [beat : Drum-Symbol (in-list pattern)])
     (case beat
       [(X)  X]
       [(O)  O]
       [(#f) pause]))))