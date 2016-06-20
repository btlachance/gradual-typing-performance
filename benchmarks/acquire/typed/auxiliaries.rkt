#lang typed/racket/base

(provide
 (contract-out
  [randomly-pick (->/c (listof any/c) any/c)]

  [distinct (->/c list? boolean?)]
  ;; for small lists O(n^2)

  ;; (->* [(Listof Any) (-> Any Real)] [#:info (-> Any Any)] (Listof (Listof Any)))
  ;; Precondition: input list is sorted vis <= or >=

  [aux:partition
   (->i ([l (key) (and/c (listof any/c)
                         (or/c
                          (sorted/k <= key)
                          (sorted/k >= key)))]
         [key (->/c any/c real?)]
         (info (->/c any/c any/c)))
        (partitions (listof (listof any/c))))]

  [sorted
   (->i ((cmp (->/c any/c any/c any/c)))
        (ok? (->/c list? any/c)))]

  [sorted/k
   (->i ((cmp (->/c any/c any/c any/c))
         (key (->/c any/c any/c)))
        (ok? (->/c list? any/c)))])
 ==>
 )

;; -----------------------------------------------------------------------------

(require
  (only-in racket/list
    first second
    rest range cons? empty?)
  )

(define-syntax-rule (==> a b) (if a b #t))

;; ---------------------------------------------------------------------------------------------------

(: randomly-pick (All (A) (-> (Listof A) A)))
(define (randomly-pick l)
  (list-ref l (random (length l))))

(: aux:partition (All (A B) (-> (Listof A) (-> A Real) (-> A B) (Listof (Listof B)))))
(define (aux:partition lo-h-size selector info)
  (define one (first lo-h-size))
  (let loop : (Listof (Listof B))
       [(pred : Real (selector one))
        (l    : (Listof A) (rest lo-h-size))
        [pt   : (Listof B) (list (info one))]]
    (cond
      [(null? l) (list (reverse pt))]
      [else 
       (define two (first l))
       (if (not (= (selector two) pred))
           (cons (reverse pt) (loop (selector two) (rest l) (list (info two))))
           (loop pred (rest l) (cons (info two) pt)))])))

(: distinct (-> (Listof Any) Boolean))
(define (distinct s)
  (cond
    [(null? s) #t]
    [else (and (not (member (first s) (rest s))) (distinct (rest s)))]))

(: sorted (All (a) (a a -> Any) -> (-> (Listof a) Boolean)))
(define ((sorted cmp) l)
  (or (empty? l)
      (let all ([l (rest l)] [pred (first l)])
        (cond
          [(empty? l) #t]
          [else
           (define key2 (first l))
           (and (cmp pred key2) (all (rest l) key2))]))))

(: sorted/k (All (a b) (b b -> Any) (a -> b) -> (-> (Listof a) Boolean)))
(define ((sorted/k cmp key) l)
  (or (empty? l)
      (let all ([l (rest l)] [pred (key (first l))])
        (cond
          [(empty? l) #t]
          [else
           (define key2 (key (first l)))
           (and (cmp pred key2) (all (rest l) key2))]))))
