#lang typed/racket/base

;; Resolving offsets between moments

(require
  ;benchmark-util
  "tzinfo-adapter.rkt"
  "core-adapter.rkt"
  "gregor-adapter.rkt"
  racket/match
  "hmsn.rkt"
  "datetime.rkt"
  "moment-base.rkt")
#;(require/typed "hmsn.rkt"
    [NS/SECOND Natural]
)
#;(require/typed "datetime.rkt"
    [datetime->iso8601 (-> DateTime String)]
    [posix->datetime (-> Exact-Rational DateTime)]
    [datetime->posix (-> DateTime Exact-Rational)]
    [datetime (->* (Natural) (Month Natural Natural Natural Natural Natural) DateTime)]
    [datetime->jd (-> DateTime Exact-Rational)]
    [datetime-add-seconds (-> DateTime Integer DateTime)]
)
#;(require/typed "moment-base.rkt"
    [make-moment (-> DateTime Integer (U String #f) Moment)]
    [moment->iso8601 (-> Moment String)]
    [moment->iso8601/tzid (-> Moment String)]
)

;; -----------------------------------------------------------------------------

(provide
 (contract-out
  [resolve-gap/pre gap-resolver/c]
  [resolve-gap/post gap-resolver/c]
  [resolve-gap/push gap-resolver/c]

  [resolve-overlap/pre overlap-resolver/c]
  [resolve-overlap/post overlap-resolver/c]
  [resolve-overlap/retain overlap-resolver/c]

  [resolve-offset/pre offset-resolver/c]
  [resolve-offset/post offset-resolver/c]
  [resolve-offset/post-gap/pre-overlap offset-resolver/c]
  [resolve-offset/retain offset-resolver/c]
  [resolve-offset/push offset-resolver/c]
  [resolve-offset/raise offset-resolver/c]

  [offset-resolver (->/c gap-resolver/c overlap-resolver/c offset-resolver/c)]
  [offset-resolver/c any/c]))
;; =============================================================================

;; -- from exn.rkt

(struct exn:gregor exn:fail ())
(struct exn:gregor:invalid-offset exn:gregor ())

(: raise-invalid-offset (-> Any DateTime Any Any Moment))
(define (raise-invalid-offset g/o target-dt target-tzid orig)
  (raise
   (exn:gregor:invalid-offset
    (format "Illegal moment: local time ~a ~a in time zone ~a"
            (datetime->iso8601 target-dt)
            (if (tzgap? g/o)
                "does not exist"
                "is ambiguous")
            target-tzid)
    (current-continuation-marks))))

;; -- from `offset-resolvers.rkt`

(: resolve-gap/pre (-> tzgap DateTime (U String #f) (U #f Moment) Moment))
(define (resolve-gap/pre gap target-dt target-tzid orig)
  ;(define tm (tzgap-starts-at gap))
  ;(define delta (tzoffset-utc-seconds (tzgap-offset-before gap)))
  (match-define (tzgap tm (tzoffset delta _ _) _) gap)
  (make-moment (posix->datetime (+ tm delta (- (/ 1 NS/SECOND)))) delta target-tzid))

(: resolve-gap/post (-> tzgap DateTime (U String #f) (U #f Moment) Moment))
(define (resolve-gap/post gap target-dt target-tzid orig)
  ;(match-define (tzgap _ _ (tzoffset delta _ _)) gap)
  (define tm (tzgap-starts-at gap))
  (define delta (tzoffset-utc-seconds (tzgap-offset-before gap)))
  (define sum (+ tm delta))
  (make-moment (posix->datetime sum) delta target-tzid))

(: resolve-gap/push (-> tzgap DateTime (U String #f) (U #f Moment) Moment))
(define (resolve-gap/push gap target-dt target-tzid orig)
  ;(match-define (tzgap tm (tzoffset delta1 _ _) (tzoffset delta2 _ _)) gap)
  (define tm (tzgap-starts-at gap))
  (define delta1 (tzoffset-utc-seconds (tzgap-offset-before gap)))
  (define delta2 (tzoffset-utc-seconds (tzgap-offset-after gap)))
  (make-moment (posix->datetime (+ (datetime->posix target-dt) (- delta2 delta1))) delta2 target-tzid))

(: resolve-overlap/pre (-> tzoverlap DateTime (U String #f) (U #f Moment) Moment))
(define (resolve-overlap/pre overlap target-dt target-tzid orig)
  (match-define (tzoverlap (tzoffset delta _ _) _) overlap)
  (make-moment target-dt delta target-tzid))

(: resolve-overlap/post (-> tzoverlap DateTime (U String #f) (U #f Moment) Moment))
(define (resolve-overlap/post overlap target-dt target-tzid orig)
  (match-define (tzoverlap _ (tzoffset delta _ _)) overlap)
  (make-moment target-dt delta target-tzid))

(: resolve-overlap/retain (-> tzoverlap DateTime (U String #f) (U #f Moment) Moment))
(define (resolve-overlap/retain overlap target-dt target-tzid orig)
  ;(match-define (tzoverlap (tzoffset delta1 _ _) (tzoffset delta2 _ _)) overlap)
  (define delta1 (tzoffset-utc-seconds (tzoverlap-offset-before overlap)))
  (define delta2 (tzoffset-utc-seconds (tzoverlap-offset-after overlap)))
  (make-moment target-dt
               (or (and orig (= (Moment-utc-offset orig) delta1) delta1)
                   delta2)
               target-tzid))

(: offset-resolver (-> (-> tzgap DateTime (U String #f) (U #f Moment) Moment)
                       (-> tzoverlap DateTime (U String #f) (U #f Moment) Moment)
                       (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment)))
(define (offset-resolver rg ro)
  (λ ([g/o : (U tzgap tzoverlap)]
      [target-dt : DateTime]
      [target-tzid : (U String #f)]
      [orig : (U #f Moment)])
    (cond [(tzgap? g/o)
           (rg g/o target-dt target-tzid orig)]
          [else
           (ro g/o target-dt target-tzid orig)])))
    ;; (define fn (if (tzgap? g/o) rg ro))
    ;; (fn g/o target-dt target-tzid orig)))

(: resolve-offset/pre (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
(define resolve-offset/pre
  (offset-resolver resolve-gap/pre resolve-overlap/pre))

(: resolve-offset/post (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
(define resolve-offset/post
  (offset-resolver resolve-gap/post resolve-overlap/post))

(: resolve-offset/post-gap/pre-overlap (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
(define resolve-offset/post-gap/pre-overlap
  (offset-resolver resolve-gap/post resolve-overlap/pre))

(: resolve-offset/retain (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
(define resolve-offset/retain
  (offset-resolver resolve-gap/post
                   resolve-overlap/retain))

(: resolve-offset/push (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
(define resolve-offset/push
  (offset-resolver resolve-gap/push
                   resolve-overlap/post))

(: resolve-offset/raise (-> (U tzgap tzoverlap) DateTime (U String #f) (U Moment #f) Moment))
(define (resolve-offset/raise g/o target-dt target-tzid orig)
  (raise-invalid-offset g/o target-dt target-tzid orig))

(define gap-resolver/c
  (->/c tzgap?
      DateTime?
      string?
      (or/c Moment? #f)
      Moment?))

(define overlap-resolver/c
  (->/c tzoverlap?
      DateTime?
      string?
      (or/c Moment? #f)
      Moment?))

(define offset-resolver/c
  (->/c (or/c tzgap? tzoverlap?)
      DateTime?
      string?
      (or/c Moment? #f)
      Moment?))
