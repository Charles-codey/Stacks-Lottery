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