#lang racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for internal game states 
;; -- inspecting the game state 
;; -- manipulating the game state 
;; including a data structure for internalizing the state of the players

(provide
 score?

 (contract-out
  [player? (->/c any/c boolean?)]
  [player-money (->/c player? cash?)]
  [player-tiles (->/c player? (listof tile?))]
  [player-shares (->/c player? shares?)]
  [player-external (->/c player? any/c)]
  [player-name (->/c player? string?)])
 ; player?
 ; ;; (-> Any Boolean)
 ; player-money
 ; ;; (-> Player Cash)
 ; player-tiles
 ; ;; (-> Player (Listof Tile))
 ; player-shares
 ; ;; (-> Player Shares)
 ; player-external
 ; ;; (-> Player Any)
 ; player-name
 ; ;; (-> Player String)

 player0
 ;; (->* (String Tile Tile Tile Tile Tile Tile) (Any) Player)

 (contract-out 
  [*create-player
   (->/c string? cash? shares? (and/c [listof tile?] distinct (λ (x) (<= (length x) STARTER-TILES#))) 
         player?)]
  ;; (-> String Cash Shares (Listof Tile) Player)
  ;; Precondition: distinct tiles
  ;; Precondition: (<= STARTER-TILES# (length tiles))

  [state? (->/c any/c boolean?)]
  [state-hotels (->/c state? (listof hotel?))]
  [state-shares (->/c state? shares?)]
  [state-sub-shares (->/c state? shares? state?)]
  [state-tiles (->/c state? (listof tile?))]
  [state-board (->/c state? board?)]
  [state-players (->/c state? (listof player?))]
  [state-current-player (->/c state? player?)]
  ;; state?
  ;; ;; (-> Any Boolean)
  ;; state-hotels
  ;; ;; (-> Any Boolean)
  ;; state-shares
  ;; ;; (-> State Shares)
  ;; state-tiles
  ;; ;; (-> State (Listof Tile))
  ;; state-board
  ;; ;; (-> State Board)
  ;; state-players
  ;; ;; (-> State (Listof Player))

  [state0
   (->i () () #:rest [rest (and/c (listof player?) (λ (l) (distinct (apply append (map player-tiles l)))))]
        [result state?])])
 ;; (->* () () #:rest (Listof Player) State)
 ;; Precondition: (λ (l) (distinct (apply append (map player-tiles l)))))

 (rename-out [ext:state-place-tile state-place-tile])
 ;; (->* (State Tile) (Hotel) State)
 ;; Precondition: (member t (player-tiles (state-current-player s)))
 ;; Precondition: (not (eq? (what-kind-of-spot (state-board s) t) IMPOSSIBLE))
 ;; Precondition: (or (unsupplied-arg? h) (memq (what-kind-of-spot (state-board s) t) (list FOUNDING MERGING)))
 ;; Precondition:
        ;; (let ((b (state-board s))
        ;;       (hotels (state-hotels s)))
        ;;   (==> (and (eq? (what-kind-of-spot b t) FOUNDING) (cons? hotels)) 
        ;;        (and (not (unsupplied-arg? h)) (member h hotels))))
 ;; Precondition:
        ;; (==> (eq? (what-kind-of-spot (state-board s) t) MERGING) 
        ;;      (and (not (unsupplied-arg? h)) 
        ;;           (let-values ([(w _) (merging-which (state-board s) t)])
        ;;             (member h w))))
 ;; Informal-Postcondition: if spot is MERGING, the hotel should be "large"

  (contract-out
  [state-buy-shares
   (->i ((s state?) (shares shares-order?))
        #:pre/name (s shares) "player has enough money" 
        (affordable? (state-board s) shares (player-money (state-current-player s)))
        #:pre/name (s shares) "shares are available"
        (let ([banker-s-shares (state-shares s)])
          (shares-available? banker-s-shares shares))
        (next-state state?))]
  ;; (-> State Shares-Order State)
  ;; Precondition:
  ;; (affordable? (state-board s) shares (player-money (state-current-player s)))
  ;; Precondition:
  ;; (let ([banker-s-shares (state-shares s)])
  ;;   (shares-available? banker-s-shares shares))

  [state-return-shares
   (->i ((s state?) (d (listof (list/c player? (listof (list/c hotel? boolean?))))))
        ;; if optional board is specified, use the hotel sizes from there
        ((b board?))
        #:pre (s d) (= (length (state-players s)) (length d))
        (next state?))]
  ;; (->* [State (Listof (List Player (Listof (List Hotel Boolean))))] [Board] State)
  ;; TODO if optional board is specified, use the hotel sizes from there
  ;; Precondition:
  ;; (= (length (state-players s)) (length d))

  [state-move-tile
   (->i ((s state?) (t tile?))
        #:pre/name (s t) "banker's tile?" (member t (state-tiles s))
        (next-state state?))]
  ;; (-> State Tile State)
  ;; Precondition:
  ;; (member t (state-tiles s))

  [state-next-turn (->/c state? state?)]
  ;; (-> State State)

  [state-remove-current-player (->/c state? state?)]
  ;; (-> State State)

  [state-eliminate (->/c state? (listof player?) state?)]
  ;; (-> State (Listof Player) State)

  [state-score (->/c state? score/c)]
  ;; (-> State Score)

  [state-final? (->/c state? boolean?)]
  ;; (-> State Boolean)

  [*create-state
   (->i ((b board?)
         (lp
          (let ()
            (define (combinable? lp)
              (define d (shares-combinable? (map player-shares lp)))
              (unless d
                (displayln `(shares not combinable ,(map player-shares lp))))
              d)
            (and/c (listof player?) combinable?))))
        #:pre/name (b lp) "players' tiles and tiles on board are mutually distinct"
        (distinct (apply append (board-tiles b) (map player-tiles lp)))
        (result state?))]
  ;; (-> Board (Listof Player) State)
  ;; Precondition: (combinable? lp)
  ;; Precondition: (distinct (apply append (board-tiles b) (map player-tiles lp)))

  [*cs0 (->i () #:rest [rest (and/c (listof (and/c string? (λ n (<= (string-length n) 20)))) distinct)]
             [result state?])])
 ;; (->* () #:rest (Listof String) State)
 ;; Precondition: strings are unique
 ;; Precondition: strings have fewer than 20 characters
 )

(define (score? x*)
  (and (list? x*)
       (for/fold
                 ([prev  #t])
                 ([x  (in-list x*)])
         (and prev
              (list? x)
              (not (null? x)) (not (null? (cdr x))) (null? (cddr x))
              (string? (car x))
              (cash? (cdr x))
              (if (cash? prev) (>= (assert prev real?) (assert (cdr x) real?)) #t)
              (assert (cdr x) exact-nonnegative-integer?)))
       #t))
(define score/c (and/c (listof (list/c string? cash?)) (sorted/k >= second)))
;; also, sorted in order of cash, descending

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: 

(require
 "../base/untyped.rkt"
 "board.rkt"
 (rename-in racket/contract [-> ->/c])
 )
(require (only-in "basics.rkt"
  ALL-HOTELS
  CASH0
  FINAL#
  SAFE#
  banker-shares0
  bonus
  cash?
  hotel?
  player-shares0
  price-per-share
  shares?
  shares++
  shares--
  shares->string
  shares-available
  shares-available?
  shares-combinable?
  shares-order?
  shares-minus
  shares-plus
))
(require (only-in "auxiliaries.rkt"
 aux:partition
 distinct
 sorted/k
 ))

;; -----------------------------------------------------------------------------
;;bg; duplicated in state-adapted.rkt

;; ---------------------------------------------------------------------------------------------------
;; DATA: 
(struct player (
  name
  tiles
  money
  shares
  external
) #:transparent)
;; Player = (player String [Listof Tile] Amount Shares)
;; Amount = Nat
;; (player t a s) is the represetation of a player 
;; -- t is the list of tiles the player owns 
;; -- a is the amount of available money 
;; -- s are the shares owned 

(define (player0 n t1 t2 t3 t4 t5 t6 x)
     (player n (list t1 t2 t3 t4 t5 t6) CASH0 player-shares0 x))

(define (*create-player name cash shares tiles)
  (player name tiles cash shares #f))

(define (player-tile- p t)
  (struct-copy player p (tiles (remove t (player-tiles p)))))

(define (player-tile+ p t)
  (struct-copy player p (tiles (cons t (player-tiles p)))))

(define (player-shares++ p . h)
  (if (empty? h)
      p
      (struct-copy player p
                   (shares (for/fold
                                     ((s  (player-shares p)))
                                     ((h  (in-list h)))
                             (shares++ s h))))))


;; Player ShareOrder Board -> Player
(define (player-buy-shares p0 sh board)
  (define amount (for/sum
                          ((h  sh))
                   ;;bg; handle failure silently
                   (or (price-per-share h (size-of-hotel board h)) 0)))
  (define m (assert (- (player-money p0) amount) exact-nonnegative-integer?))
  (apply player-shares++ (struct-copy player p0 (money m)) sh))

;; Player Shares Board -> Player
(define (player-returns-shares p0 transfers board)
  (match-define (player _name _tiles money shares _ext) p0)
  (define amount (shares->money transfers board))
  (struct-copy player p0 (money (+ money amount)) (shares (shares-minus shares transfers))))

;; ---------------------------------------------------------------------------------------------------
(struct state (
  board
  players
  tiles
  hotels
  shares
  bad
) #:transparent)
;; State  = (state Board [Listof Player] [Listof Hotel] Shares [Listof Player])
;; (state b p t h s bad) is a representation of a game state: 
;; -- b is the current board 
;; -- p is the state of all the players 
;;    the list order determines the order of turns,
;;    it is the turn of the first player on the list
;; -- t is the list of available tiles 
;; -- h is the list of available hotels 
;; -- s is the list of available shares 
;; -- bad is the list of players gone bad 

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: placing a tile and all consequences 

(define (state0 . p)
  (define tiles-owned-by-players (apply append (map player-tiles p)))
  (define tiles-in-pool (remove* tiles-owned-by-players ALL-TILES))
  (state (make-board) p tiles-in-pool ALL-HOTELS banker-shares0 '()))

(define (state-sub-shares s bad-shares)
  (struct-copy state s (shares (shares-minus (state-shares s) bad-shares))))

;; Player *-> State 
;; create players with given names, assigning random tiles from the full pool
(define (*cs0 . names)
  (let loop
       ((names  names)
        (tiles  ALL-TILES)
        (players  '()))
    (cond
      [(empty? names)
       (state (make-board) (reverse players) tiles ALL-HOTELS banker-shares0 '())]
      [else (define first-six (take tiles STARTER-TILES#))
            (define player1 (player (first names) first-six CASH0 player-shares0 #f))
            (loop (rest names) (drop tiles STARTER-TILES#) (cons player1 players))])))

;; Board [Listof Player] -> State
;; create a state by subtracting the tiles on the board and the players from the pool
;; subtracting the shares of the players from the full shares 
;; subtracting the placed hotels from the available hotels 
(define (*create-state board players)
  (define players-shares
    (for/list
              ([p  (in-list players)])
      (player-shares p)))
  (define remaining-shares 
    (for/fold
              ((remaining-shares banker-shares0))
              [(s (in-list players-shares))]
      (shares-minus remaining-shares s)))
  (define remaining-hotels 
    (for/list
              ((h  (in-list ALL-HOTELS))
               #:when (= (size-of-hotel board h) 0))
      h))
  (define remaining-tiles 
    (remove* (apply append (board-tiles board) (map player-tiles players)) ALL-TILES))
  (state board players remaining-tiles remaining-hotels remaining-shares '()))

(define (ext:state-place-tile s tile (hotel #f))
  (unless (member tile (player-tiles (state-current-player s)))
    (error 'state-place-tile (format "Precondition: tile ~a belongs to player ~a" tile (state-current-player s))))
  (unless (not (eq? (what-kind-of-spot (state-board s) tile) 'IMPOSSIBLE))
    (error 'state-place-tile (format "Precondition: impossible position for ~a on board" tile)))
  (let ([spot-type (what-kind-of-spot (state-board s) tile)])
    (when hotel ;; i.e., hotel is not unsupplied
      (unless (memq spot-type '(FOUNDING MERGING))
        (error 'state-place-tile (format "Precondition: expected founding or merging spot for ~a" tile))))
    (let ([b (state-board s)]
          [hotels (state-hotels s)])
      (unless (if (and (eq? spot-type 'FOUNDING) (pair? hotels))
                  (and hotel (member hotel hotels))
                  #t)
        (error 'state-place-tile (format "Precondition: if spot is FOUNDING and hotels are available, ~a must be one of them" hotel))))
    (unless (if (eq? spot-type 'MERGING)
                (and hotel
                     (let-values ([(w _) (merging-which (state-board s) tile)])
                       (member hotel w)))
                #t)
      (error 'state-place-tile (format "Precondition:  if tile placement causes merger, hotel ~a must be given and an acquirer" hotel))))
  (state-place-tile s tile hotel))

;; State Tile [Hotel] -> State 
;; place the tile (in the possession of player 1) onto board
;; if founding, use hotel 
;; if merging, hotel is acquirer
(define (state-place-tile s tile (hotel #f))
  (match-define (state board players tiles hotels shares _bad) s)
  (define current (player-tile- (first players) tile))
  (define others (rest players))
  (define players-next (cons current others))
  (define tiles-next (remove tile tiles))
  (define spot (what-kind-of-spot board tile))
  (cond
    [(or (eq? SINGLETON spot) (eq? GROWING spot) (and (eq? FOUNDING spot) (not hotel)))
     (define new-board 
       (if (eq? GROWING spot)
           (grow-hotel board tile)
           (place-tile board tile)))
     (struct-copy state s (board new-board) (tiles tiles-next) (players players-next))]
    [(eq? FOUNDING spot)
     (define t 
       (struct-copy state s
                    (hotels (remove hotel hotels))
                    (tiles tiles-next)
                    (board (found-hotel board tile hotel))))
     (if (= (shares-available shares hotel) 0)
      (struct-copy state t (players players-next))
      (struct-copy state t
                   (shares (shares-- shares hotel))
                   (players (cons (player-shares++ current hotel) others))))]
    [(eq? MERGING spot)
     (define-values (w l) (merging-which board tile))
     (define acquired (append (remove hotel w) l))
     (define next-state
       (struct-copy state s
                    (board (merge-hotels board tile (or hotel (error 'hotel=#f))))
                    (hotels (append acquired hotels))
                    (tiles tiles-next)
                    (players players-next)))
     ;; now distribute the bonus
     (foldr (state-distribute-bonus board) next-state acquired)]
    [else (error 'condfailed)]))

;; [Listf Hotel Nat] State -> State 
;; distribute bonus for the acquired hotel
(define ((state-distribute-bonus board) acquired-hotel s)
  (define size-acquired (size-of-hotel board acquired-hotel))
  (define players (state-players s))
  (define selector (lambda (p) (shares-available (player-shares p) acquired-hotel)))
  (define owners-of-acquired (filter (lambda (p) (> (selector p) 0)) players))
  (define owners-of-acquired-sorted
    (sort owners-of-acquired (lambda (x y) (> (selector x) (selector y)))))
  (cond
    [(empty? owners-of-acquired-sorted) s]
    [else
     (define majority-minority (aux:partition owners-of-acquired-sorted selector (lambda (x) x)))
     (define majority (first majority-minority))
     (define minority (if (empty? (rest majority-minority)) '() (second majority-minority)))
     (define majority-bonus (bonus 'majority acquired-hotel size-acquired))
     (define minority-bonus (bonus 'minority acquired-hotel size-acquired))
     (cond
       [(pair? (rest majority))
        ;; distribute the majority+minority bonus
        (define total-bonus (+ majority-bonus minority-bonus))
        (define bonus-per (quotient total-bonus (length majority)))
        (struct-copy state s (players (foldr (state-pay-out bonus-per) players majority)))]
       [(cons? minority)  ;; (empty? (rest majority))
        (define single-majority (first majority))
        (define majority-payed ((state-pay-out majority-bonus) single-majority players))
        (define bonus-per (quotient minority-bonus (length minority)))
        (struct-copy state s (players (foldr (state-pay-out bonus-per) majority-payed minority)))]
       [(empty? minority)  ;; (empty? (rest majority))
        (define single-majority (first majority))
        (struct-copy state s
                     (players ((state-pay-out majority-bonus) single-majority players)))])]))

(define (state-pay-out bonus)
  ;; add cash to wallet of pay-to in players
  (lambda (pay-to players)
    (define the-name (player-name pay-to))
    (for/list
        ((p (in-list players)))
      (match-define (player name _tiles money _shares _ext) p)
      (if (string=? name the-name) (struct-copy player p (money (+ money bonus))) p))))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: external 

(define (state-move-tile s t)
  (match-define (state _board players tiles _hotels _shares _bad) s)
  (struct-copy state s 
               (players (cons (player-tile+ (first players) t) (rest players)))
               (tiles (remove t tiles))))

(define (state-next-turn s)
  (define players (state-players s))
  (struct-copy state s (players (append (rest players) (list (first players))))))

(define (state-remove-current-player s)
  (define players (state-players s))
  (struct-copy state s (players (rest players)) (bad (cons (first players) (state-bad s)))))

(define (state-eliminate s ep)
  (struct-copy state s (players (remove* ep (state-players s))) (bad (append ep (state-bad s)))))

(define (state-current-player s)
  (first (state-players s)))

(define (state-buy-shares s sh)
  (match-define (state board players _tiles _hotels shares _bad) s)
  (struct-copy state s 
               (players (cons (player-buy-shares (first players) sh board) (rest players)))
               (shares (for/fold
                                 ((s  shares))
                                 ((h  sh))
                         (shares-- s h)))))

(define (state-return-shares s decisions (board (state-board s)))
  (for/fold
            ((s  s))
            ((d  decisions))
    (state-return-shares/player s (first d) (second d) board)))

;; State Player [Listof [List Hotel Any]] -> State 
;; return player p shares to state s according to its decisions
(define (state-return-shares/player s p p-s-decisions (board (state-board s)))
  (define the-name (player-name p))
  (define player-s (player-shares p))
  (define transfers (shares-to-be-moved+their-value player-s p-s-decisions))
  (match-define (state _board players _tiles _hotels shares _bad) s)
  (define new-players 
    (for/list
              ((q  players)) 
      (if (string=? (player-name q) the-name)
          (player-returns-shares q transfers board)
          q)))
  (struct-copy state s (shares (shares-plus shares transfers)) (players new-players)))

;; Board Shares [Listof [List Hotel Boolean]] -> Shares 
;; determine how the shares that must be transfered from player-s-shares to banker-s-shares
(define (shares-to-be-moved+their-value player-s-shares decisions)
  (for/fold
            ((shares  player-shares0))
            ((d  decisions))
    (define hotel (first d))
    (cond
      [(second d) shares]
      [else (define available (shares-available player-s-shares hotel))
            (for/fold
                      ((shares  shares))
                      ((n  (in-range available)))
              (shares++ shares hotel))])))

(define (state-score s0)
  (define board (state-board s0))
  (define bonus (state-distribute-bonus board))
  (define state/bonus
    (foldr (lambda (h s)
             (if (= (size-of-hotel board h) 0) s (bonus h s)))
           s0 ALL-HOTELS))
  (define scores 
    (for/list
              ((p  (in-list (state-players state/bonus))))
      (match-define (player name _tiles money shares _external) p)
      (list name (+ money (shares->money shares board)))))
  (sort scores 
        (lambda (p q)
          (or (> (second p) (second q)) 
              (and (= (second p) (second q)) (string<=? (first p) (first q)))))))

(define (shares->money shares board)
  (assert
  (for/sum
           ([(hotel n) (in-hash shares)])
    (define size (size-of-hotel board hotel))
    (define price (price-per-share hotel size))
    (if (and (> size 0) price) (* price n) 0)) exact-nonnegative-integer?))

(define (state-final? s)
  (define board  (state-board s))
  (define-values (winner? founded safe)
    (for/fold
              ((winner?  #f)
               (founded  '())
               (safe  '()))
              ((h  ALL-HOTELS))
      (define s (size-of-hotel board h))
      (cond
        [(>= s SAFE#) (values (>= s FINAL#) (cons h founded) (cons h safe))]
        [(> s 0) (values winner? (cons h founded) safe)]
        [else (values winner? founded safe)])))
  (or winner? (and (cons? founded) (= (length founded) (length safe)))))
