;; Stacks Lottery - Weekly lottery with STX prizes
;; Players buy tickets for a chance to win the jackpot

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_LOTTERY_NOT_FOUND (err u301))
(define-constant ERR_LOTTERY_CLOSED (err u302))
(define-constant ERR_INVALID_TICKET_COUNT (err u303))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u304))
(define-constant ERR_LOTTERY_STILL_ACTIVE (err u305))
(define-constant ERR_NO_REFUND_AVAILABLE (err u306))

(define-constant TICKET_PRICE u1000000) ;; 1 STX in microSTX

(define-data-var lottery-counter uint u0)
(define-data-var current-lottery-id uint u0)

(define-map lotteries
  { lottery-id: uint }
  {
    start-block: uint,
    end-block: uint,
    ticket-price: uint,
    total-tickets: uint,
    prize-pool: uint,
    status: (string-ascii 10),
    winner: (optional principal)
  }
)

(define-map tickets
  { lottery-id: uint, ticket-number: uint }
  { owner: principal }
)

(define-map player-tickets
  { lottery-id: uint, player: principal }
  { ticket-count: uint }
)

(define-public (create-lottery (duration-blocks uint))
  (let
    (
      (lottery-id (+ (var-get lottery-counter) u1))
      (start-block block-height)
      (end-block (+ block-height duration-blocks))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set lottery-counter lottery-id)
    (var-set current-lottery-id lottery-id)
    (map-set lotteries
      { lottery-id: lottery-id }
      {
        start-block: start-block,
        end-block: end-block,
        ticket-price: TICKET_PRICE,
        total-tickets: u0,
        prize-pool: u0,
        status: "open",
        winner: none
      }
    )
    (ok lottery-id)
  )
)

(define-public (buy-tickets (lottery-id uint) (ticket-count uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (total-cost (* ticket-count TICKET_PRICE))
      (current-tickets (get total-tickets lottery))
    )
    (asserts! (is-eq (get status lottery) "open") ERR_LOTTERY_CLOSED)
    (asserts! (< block-height (get end-block lottery)) ERR_LOTTERY_CLOSED)
    (asserts! (and (> ticket-count u0) (<= ticket-count u10)) ERR_INVALID_TICKET_COUNT)
    
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    (map-set lotteries
      { lottery-id: lottery-id }
      (merge lottery {
        total-tickets: (+ current-tickets ticket-count),
        prize-pool: (+ (get prize-pool lottery) total-cost)
      })
    )
    
    (assign-tickets lottery-id current-tickets ticket-count tx-sender)
    
    (let
      (
        (player-current-tickets (default-to { ticket-count: u0 }
                                  (map-get? player-tickets { lottery-id: lottery-id, player: tx-sender })))
      )
      (map-set player-tickets
        { lottery-id: lottery-id, player: tx-sender }
        { ticket-count: (+ (get ticket-count player-current-tickets) ticket-count) }
      )
    )
    
    (ok ticket-count)
  )
)

(define-private (assign-tickets (lottery-id uint) (start-ticket uint) (count uint) (owner principal))
  (if (is-eq count u0)
    true
    (begin
      (map-set tickets
        { lottery-id: lottery-id, ticket-number: start-ticket }
        { owner: owner }
      )
      (assign-tickets lottery-id (+ start-ticket u1) (- count u1) owner)
    )
  )
)

(define-public (draw-winner (lottery-id uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (total-tickets (get total-tickets lottery))
      (winning-ticket (mod (unwrap-panic (get-block-info? vrf-seed block-height)) total-tickets))
      (winner-data (unwrap! (map-get? tickets { lottery-id: lottery-id, ticket-number: winning-ticket }) ERR_LOTTERY_NOT_FOUND))
      (winner (get owner winner-data))
      (prize-amount (get prize-pool lottery))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status lottery) "open") ERR_LOTTERY_CLOSED)
    (asserts! (>= block-height (get end-block lottery)) ERR_LOTTERY_CLOSED)
    (asserts! (> total-tickets u0) ERR_LOTTERY_NOT_FOUND)
    
    (map-set lotteries
      { lottery-id: lottery-id }
      (merge lottery {
        status: "closed",
        winner: (some winner)
      })
    )
    
    (try! (as-contract (stx-transfer? prize-amount tx-sender winner)))
    (ok { winner: winner, prize: prize-amount, winning-ticket: winning-ticket })
  )
)

;; NEW FUNCTION 1: Cancel lottery and refund players (emergency function)
(define-public (cancel-lottery (lottery-id uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status lottery) "open") ERR_LOTTERY_CLOSED)
    
    (map-set lotteries
      { lottery-id: lottery-id }
      (merge lottery {
        status: "cancelled"
      })
    )
    
    (ok true)
  )
)

;; NEW FUNCTION 2: Claim refund for cancelled lottery
(define-public (claim-refund (lottery-id uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (player-data (unwrap! (map-get? player-tickets { lottery-id: lottery-id, player: tx-sender }) ERR_NO_REFUND_AVAILABLE))
      (refund-amount (* (get ticket-count player-data) TICKET_PRICE))
    )
    (asserts! (is-eq (get status lottery) "cancelled") ERR_NO_REFUND_AVAILABLE)
    (asserts! (> refund-amount u0) ERR_NO_REFUND_AVAILABLE)
    
    ;; Mark player as refunded by setting ticket count to 0
    (map-set player-tickets
      { lottery-id: lottery-id, player: tx-sender }
      { ticket-count: u0 }
    )
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    (ok refund-amount)
  )
)

;; NEW FUNCTION 3: Get lottery statistics and history
(define-read-only (get-lottery-stats (lottery-id uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (total-tickets (get total-tickets lottery))
      (blocks-remaining (if (> (get end-block lottery) block-height)
                          (- (get end-block lottery) block-height)
                          u0))
      (is-active (and (is-eq (get status lottery) "open") 
                      (< block-height (get end-block lottery))))
    )
    (ok {
      lottery-id: lottery-id,
      status: (get status lottery),
      start-block: (get start-block lottery),
      end-block: (get end-block lottery),
      blocks-remaining: blocks-remaining,
      is-active: is-active,
      total-tickets: total-tickets,
      prize-pool: (get prize-pool lottery),
      ticket-price: (get ticket-price lottery),
      winner: (get winner lottery),
      participation-rate: (if (> total-tickets u0) 
                           (/ (* total-tickets u100) (+ total-tickets u1)) 
                           u0)
    })
  )
)

;; NEW FUNCTION 4: Get all player ticket numbers for a specific lottery
(define-read-only (get-player-ticket-numbers (lottery-id uint) (player principal))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (player-data (map-get? player-tickets { lottery-id: lottery-id, player: player }))
      (total-tickets (get total-tickets lottery))
    )
    (match player-data
      player-info
      (ok (collect-player-tickets lottery-id player u0 total-tickets (list)))
      (ok (list))
    )
  )
)

;; Helper function to collect all ticket numbers owned by a player
(define-private (collect-player-tickets (lottery-id uint) (player principal) (current-ticket uint) (max-tickets uint) (ticket-list (list 100 uint)))
  (if (>= current-ticket max-tickets)
    ticket-list
    (let
      (
        (ticket-owner (map-get? tickets { lottery-id: lottery-id, ticket-number: current-ticket }))
      )
      (match ticket-owner
        owner-data
        (if (is-eq (get owner owner-data) player)
          (collect-player-tickets lottery-id player (+ current-ticket u1) max-tickets (unwrap-panic (as-max-len? (append ticket-list current-ticket) u100)))
          (collect-player-tickets lottery-id player (+ current-ticket u1) max-tickets ticket-list))
        (collect-player-tickets lottery-id player (+ current-ticket u1) max-tickets ticket-list)
      )
    )
  )
)

(define-read-only (get-lottery (lottery-id uint))
  (map-get? lotteries { lottery-id: lottery-id })
)

(define-read-only (get-current-lottery)
  (var-get current-lottery-id)
)

(define-read-only (get-player-tickets (lottery-id uint) (player principal))
  (map-get? player-tickets { lottery-id: lottery-id, player: player })
)

(define-read-only (get-ticket-owner (lottery-id uint) (ticket-number uint))
  (map-get? tickets { lottery-id: lottery-id, ticket-number: ticket-number })
)

(define-read-only (get-all-lotteries)
  (map-values lotteries)
)

(define-read-only (get-all-player-tickets (player principal))
  (let
    (
      (all-tickets (map-values player-tickets))
      (player-tickets (filter (fn (ticket) (is-eq (get player ticket) player)) all-tickets))
    )
    (ok player-tickets)
  )
)

(define-read-only (get-lottery-history)
  (let
    (
      (all-lotteries (map-values lotteries))
      (closed-lotteries (filter (fn (lottery) (is-eq (get status lottery) "closed")) all-lotteries))
    )
    (ok closed-lotteries)
  )
)