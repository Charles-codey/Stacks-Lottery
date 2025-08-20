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