#lang typed/racket/base

(require racket/vector
         (only-in racket/fixnum fx+)
         (only-in "array-struct.rkt"
                  array-shape
                  unsafe-array-proc
                  unsafe-build-array
                  array-default-strict)
         (only-in "array-broadcast.rkt" array-broadcast array-shape-broadcast)
         (only-in "array-utils.rkt" unsafe-vector-remove vector-copy-all unsafe-vector-insert)
         "array-types.rkt")

(provide array-append*)

(: array-broadcast-for-append (All (A) ((Listof (Array A))
                                        Integer -> (Values (Listof (Array A))
                                                           (Listof Index)))))
(define (array-broadcast-for-append arrs k)
  (define dss (map (λ: ([arr : (Array A)]) (array-shape arr)) arrs))
  (define dims (apply max (map vector-length dss)))
  (cond [(not (index? dims))  (error 'array-broadcast-for-append "can't happen")]
        [(or (k . < . 0) (k . >= . dims))
         (raise-argument-error 'array-append* (format "Index < ~a" dims) k)]
        [else
         (let* ([dss  (map (λ: ([ds : Indexes])
                             (define dms (vector-length ds))
                             (vector-append ((inst make-vector Index) (- dims dms) 1) ds))
                           dss)]
                [dks  (map (λ: ([ds : Indexes]) (vector-ref ds k)) dss)]
                [dss  (map (λ: ([ds : Indexes]) (unsafe-vector-remove ds k)) dss)]
                [ds   (array-shape-broadcast dss)]
                [dss  (map (λ: ([dk : Index]) (unsafe-vector-insert ds k dk)) dks)])
           (define new-arrs
             (map (λ: ([arr : (Array A)] [ds : Indexes]) (array-broadcast arr ds)) arrs dss))
           (values new-arrs dks))]))

(: array-append* (All (A) (case-> ((Listof (Array A)) -> (Array A))
                                  ((Listof (Array A)) Integer -> (Array A)))))
(define (array-append* arrs [k 0])
  (when (null? arrs) (raise-argument-error 'array-append* "nonempty (Listof (Array A))" arrs))
  (let-values ([(arrs dks)  (array-broadcast-for-append arrs k)])
    (define new-dk (apply + dks))
    (cond
      [(not (index? new-dk))  (error 'array-append* "resulting axis is too large (not an Index)")]
      [else
       (define dss (map (λ: ([arr : (Array A)]) (array-shape arr)) arrs))
       (define new-ds (vector-copy-all (car dss)))
       (vector-set! new-ds k new-dk)
       ;; Make two mappings:
       ;; 1. old-procs : new array index -> old array procedure
       ;; 2. old-jks :   new array index -> old array index
       (define old-procs (make-vector new-dk (unsafe-array-proc (car arrs))))
       (define: old-jks : Indexes (make-vector new-dk 0))
       (let arrs-loop ([arrs arrs] [dks dks] [#{jk : Nonnegative-Fixnum} 0])
         (unless (null? arrs)
           (define arr (car arrs))
           (define proc (unsafe-array-proc arr))
           (define dk (car dks))
           (let i-loop ([#{i : Nonnegative-Fixnum} 0] [#{jk : Nonnegative-Fixnum} jk])
             (cond [(i . < . dk)  (vector-set! old-procs jk proc)
                                  (vector-set! old-jks jk i)
                                  (i-loop (+ i 1) (fx+ jk 1))]
                   [else  (arrs-loop (cdr arrs) (cdr dks) jk)]))))
       (array-default-strict
        (unsafe-build-array
         new-ds (λ: ([js : Indexes])
                  (define jk (vector-ref js k))
                  (vector-set! js k (vector-ref old-jks jk))
                  (define v ((vector-ref old-procs jk) js))
                  (vector-set! js k jk)
                  v)))])))