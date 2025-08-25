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

;; Helper function to assign a single ticket
(define-private (assign-single-ticket (lottery-id uint) (ticket-number uint) (owner principal))
  (map-set tickets
    { lottery-id: lottery-id, ticket-number: ticket-number }
    { owner: owner }
  )
)

;; Helper function to assign tickets using fold
(define-private (assign-tickets-fold (item uint) (data { lottery-id: uint, start-ticket: uint, owner: principal }))
  (let
    (
      (current-ticket (+ (get start-ticket data) item))
    )
    (assign-single-ticket (get lottery-id data) current-ticket (get owner data))
    data
  )
)

;; Helper function to create a list of numbers from 0 to count-1
(define-private (create-range (count uint))
  (if (<= count u1)
    (list u0)
    (if (is-eq count u2)
      (list u0 u1)
      (if (is-eq count u3)
        (list u0 u1 u2)
        (if (is-eq count u4)
          (list u0 u1 u2 u3)
          (if (is-eq count u5)
            (list u0 u1 u2 u3 u4)
            (if (is-eq count u6)
              (list u0 u1 u2 u3 u4 u5)
              (if (is-eq count u7)
                (list u0 u1 u2 u3 u4 u5 u6)
                (if (is-eq count u8)
                  (list u0 u1 u2 u3 u4 u5 u6 u7)
                  (if (is-eq count u9)
                    (list u0 u1 u2 u3 u4 u5 u6 u7 u8)
                    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Main function to assign multiple tickets
(define-private (assign-tickets (lottery-id uint) (start-ticket uint) (count uint) (owner principal))
  (let
    (
      (range-list (create-range count))
      (fold-data { lottery-id: lottery-id, start-ticket: start-ticket, owner: owner })
    )
    (fold assign-tickets-fold range-list fold-data)
    true
  )
)

(define-public (create-lottery (duration-blocks uint))
  (let
    (
      (lottery-id (+ (var-get lottery-counter) u1))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration-blocks))
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
    (asserts! (< stacks-block-height (get end-block lottery)) ERR_LOTTERY_CLOSED)
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

(define-public (draw-winner (lottery-id uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (total-tickets (get total-tickets lottery))
      (winning-ticket (mod (+ stacks-block-height lottery-id) total-tickets))
      (winner-data (unwrap! (map-get? tickets { lottery-id: lottery-id, ticket-number: winning-ticket }) ERR_LOTTERY_NOT_FOUND))
      (winner (get owner winner-data))
      (prize-amount (get prize-pool lottery))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status lottery) "open") ERR_LOTTERY_CLOSED)
    (asserts! (>= stacks-block-height (get end-block lottery)) ERR_LOTTERY_CLOSED)
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

;; Cancel lottery and refund players (emergency function)
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

;; Claim refund for cancelled lottery
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

;; Read-only functions

(define-read-only (get-lottery-stats (lottery-id uint))
  (let
    (
      (lottery (unwrap! (map-get? lotteries { lottery-id: lottery-id }) ERR_LOTTERY_NOT_FOUND))
      (total-tickets (get total-tickets lottery))
      (blocks-remaining (if (> (get end-block lottery) stacks-block-height)
                          (- (get end-block lottery) stacks-block-height)
                          u0))
      (is-active (and (is-eq (get status lottery) "open") 
                      (< stacks-block-height (get end-block lottery))))
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

;; Simplified function to check if a player owns a specific ticket
(define-read-only (player-owns-ticket (lottery-id uint) (player principal) (ticket-number uint))
  (match (map-get? tickets { lottery-id: lottery-id, ticket-number: ticket-number })
    ticket-data (is-eq (get owner ticket-data) player)
    false
  )
)

;; Get basic player statistics for a lottery
(define-read-only (get-player-lottery-info (lottery-id uint) (player principal))
  (let
    (
      (lottery (map-get? lotteries { lottery-id: lottery-id }))
      (player-data (map-get? player-tickets { lottery-id: lottery-id, player: player }))
    )
    (match lottery
      lottery-info
      (match player-data
        player-info
        (ok {
          lottery-id: lottery-id,
          player: player,
          ticket-count: (get ticket-count player-info),
          total-spent: (* (get ticket-count player-info) TICKET_PRICE),
          lottery-status: (get status lottery-info),
          total-lottery-tickets: (get total-tickets lottery-info)
        })
        (ok {
          lottery-id: lottery-id,
          player: player,
          ticket-count: u0,
          total-spent: u0,
          lottery-status: (get status lottery-info),
          total-lottery-tickets: (get total-tickets lottery-info)
        })
      )
      ERR_LOTTERY_NOT_FOUND
    )
  )
)