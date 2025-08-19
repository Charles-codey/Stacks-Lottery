# Stacks Lottery

A transparent, blockchain-based lottery system where players buy tickets for a chance to win STX prizes.

## Features

- Weekly lottery draws with STX prizes
- Transparent ticket assignment
- VRF-based random winner selection
- Multiple tickets per player allowed
- Complete prize pool distribution to winner

## How to Play

1. Wait for lottery to open (created by contract owner)
2. Buy tickets using `buy-tickets` (1 STX per ticket, max 10 per transaction)
3. Wait for lottery end block
4. Winner is randomly selected and receives entire prize pool

## Smart Contract Functions

- `create-lottery`: Start new lottery (owner only)
- `buy-tickets`: Purchase lottery tickets (1-10 per transaction)
- `draw-winner`: Select random winner (owner only)
- `get-lottery`: View lottery details
- `get-player-tickets`: Check your ticket count
- `get-current-lottery`: Get active lottery ID

## Game Economics

- Ticket Price: 1 STX each
- Maximum 10 tickets per transaction
- 100% of ticket sales go to winner
- Provably fair random selection using VRF