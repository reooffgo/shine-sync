;; ShineSync - Creator Economy Platform
;; A decentralized platform for dynamic NFT collectibles and micro-patronage

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-invalid-stage (err u106))

;; Evolution stages
(define-constant stage-spark u0)
(define-constant stage-flicker u1)
(define-constant stage-glow u2)
(define-constant stage-radiance u3)
(define-constant stage-brilliance u4)
(define-constant stage-nova u5)
(define-constant stage-supernova u6)

;; Revenue split percentages (in basis points, 10000 = 100%)
(define-constant creator-share u7000)  ;; 70%
(define-constant fan-reward-share u2000)  ;; 20%
(define-constant platform-share u1000)  ;; 10%

;; Data Variables
(define-data-var token-id-nonce uint u0)
(define-data-var platform-treasury principal contract-owner)

;; Data Maps
(define-map creators principal {
  total-tokens: uint,
  total-revenue: uint,
  active: bool
})

(define-map shine-tokens uint {
  creator: principal,
  owner: principal,
  stage: uint,
  engagement-points: uint,
  created-at: uint,
  last-evolved: uint
})

(define-map token-ownership principal (list 200 uint))

(define-map engagement-proofs {token-id: uint, proof-id: uint} {
  fan: principal,
  points: uint,
  timestamp: uint,
  proof-hash: (buff 32)
})

(define-map creator-balances principal uint)
(define-map fan-reward-pools principal uint)

;; Private Functions
(define-private (calculate-share (amount uint) (share-percentage uint))
  (/ (* amount share-percentage) u10000)
)

;; Read-only Functions
(define-read-only (get-token (token-id uint))
  (map-get? shine-tokens token-id)
)

(define-read-only (get-creator-info (creator principal))
  (map-get? creators creator)
)

(define-read-only (get-token-owner (token-id uint))
  (match (map-get? shine-tokens token-id)
    token (ok (get owner token))
    (err err-not-found)
  )
)

(define-read-only (get-creator-balance (creator principal))
  (default-to u0 (map-get? creator-balances creator))
)

(define-read-only (get-fan-reward-pool (creator principal))
  (default-to u0 (map-get? fan-reward-pools creator))
)

(define-read-only (get-tokens-by-owner (owner principal))
  (default-to (list) (map-get? token-ownership owner))
)

(define-read-only (get-stage-name (stage uint))
  (if (is-eq stage stage-spark) "Spark"
  (if (is-eq stage stage-flicker) "Flicker"
  (if (is-eq stage stage-glow) "Glow"
  (if (is-eq stage stage-radiance) "Radiance"
  (if (is-eq stage stage-brilliance) "Brilliance"
  (if (is-eq stage stage-nova) "Nova"
  (if (is-eq stage stage-supernova) "Supernova"
  "Unknown")))))))
)

;; Public Functions

;; Register as a creator
(define-public (register-creator)
  (let ((existing-creator (map-get? creators tx-sender)))
    (if (is-some existing-creator)
      (err err-already-exists)
      (begin
        (map-set creators tx-sender {
          total-tokens: u0,
          total-revenue: u0,
          active: true
        })
        (ok true)
      )
    )
  )
)

;; Mint a new Shine Token
(define-public (mint-shine-token (fan principal))
  (let (
    (new-token-id (+ (var-get token-id-nonce) u1))
    (creator-data (unwrap! (map-get? creators tx-sender) (err err-unauthorized)))
    (current-tokens (default-to (list) (map-get? token-ownership fan)))
  )
    (asserts! (get active creator-data) (err err-unauthorized))
    (unwrap! (stx-transfer? u1000000 fan tx-sender) (err err-insufficient-balance))
    
    (map-set shine-tokens new-token-id {
      creator: tx-sender,
      owner: fan,
      stage: stage-spark,
      engagement-points: u0,
      created-at: block-height,
      last-evolved: block-height
    })
    
    (map-set token-ownership fan (unwrap-panic (as-max-len? (append current-tokens new-token-id) u200)))
    
    (map-set creators tx-sender (merge creator-data {
      total-tokens: (+ (get total-tokens creator-data) u1)
    }))
    
    (var-set token-id-nonce new-token-id)
    (ok new-token-id)
  )
)

;; Support creator with micro-patronage
(define-public (support-creator (creator principal) (amount uint))
  (let (
    (creator-data (unwrap! (map-get? creators creator) (err err-not-found)))
    (creator-amt (calculate-share amount creator-share))
    (fan-reward-amt (calculate-share amount fan-reward-share))
    (platform-amt (calculate-share amount platform-share))
    (current-creator-balance (default-to u0 (map-get? creator-balances creator)))
    (current-fan-pool (default-to u0 (map-get? fan-reward-pools creator)))
  )
    (asserts! (> amount u0) (err err-invalid-amount))
    (asserts! (get active creator-data) (err err-unauthorized))
    
    ;; Transfer funds
    (unwrap! (stx-transfer? creator-amt tx-sender creator) (err err-insufficient-balance))
    (unwrap! (stx-transfer? platform-amt tx-sender (var-get platform-treasury)) (err err-insufficient-balance))
    
    ;; Update balances
    (map-set creator-balances creator (+ current-creator-balance creator-amt))
    (map-set fan-reward-pools creator (+ current-fan-pool fan-reward-amt))
    
    (map-set creators creator (merge creator-data {
      total-revenue: (+ (get total-revenue creator-data) amount)
    }))
    
    (ok true)
  )
)

;; Add engagement points to token
(define-public (add-engagement (token-id uint) (points uint) (proof-hash (buff 32)))
  (let (
    (token-data (unwrap! (map-get? shine-tokens token-id) (err err-not-found)))
    (new-points (+ (get engagement-points token-data) points))
    (proof-id (+ token-id block-height))
  )
    (asserts! (is-eq tx-sender (get owner token-data)) (err err-unauthorized))
    (asserts! (> points u0) (err err-invalid-amount))
    
    ;; Store engagement proof
    (map-set engagement-proofs {token-id: token-id, proof-id: proof-id} {
      fan: tx-sender,
      points: points,
      timestamp: block-height,
      proof-hash: proof-hash
    })
    
    ;; Update token
    (map-set shine-tokens token-id (merge token-data {
      engagement-points: new-points
    }))
    
    (ok new-points)
  )
)

;; Evolve token to next stage
(define-public (evolve-token (token-id uint))
  (let (
    (token-data (unwrap! (map-get? shine-tokens token-id) (err err-not-found)))
    (current-stage (get stage token-data))
    (engagement (get engagement-points token-data))
    (required-points (* (+ current-stage u1) u100))  ;; Progressive requirements
  )
    (asserts! (is-eq tx-sender (get creator token-data)) (err err-unauthorized))
    (asserts! (< current-stage stage-supernova) (err err-invalid-stage))
    (asserts! (>= engagement required-points) (err err-invalid-amount))
    
    (map-set shine-tokens token-id (merge token-data {
      stage: (+ current-stage u1),
      last-evolved: block-height
    }))
    
    (ok (+ current-stage u1))
  )
)

;; Transfer token ownership
(define-public (transfer-token (token-id uint) (new-owner principal))
  (let (
    (token-data (unwrap! (map-get? shine-tokens token-id) (err err-not-found)))
    (current-owner-tokens (default-to (list) (map-get? token-ownership tx-sender)))
    (new-owner-tokens (default-to (list) (map-get? token-ownership new-owner)))
  )
    (asserts! (is-eq tx-sender (get owner token-data)) (err err-unauthorized))
    
    ;; Update token owner
    (map-set shine-tokens token-id (merge token-data {
      owner: new-owner
    }))
    
    ;; Update ownership lists
    (map-set token-ownership new-owner 
      (unwrap-panic (as-max-len? (append new-owner-tokens token-id) u200)))
    
    (ok true)
  )
)

;; Admin: Update platform treasury
(define-public (set-platform-treasury (new-treasury principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (var-set platform-treasury new-treasury)
    (ok true)
  )
)

;; Admin: Deactivate creator
(define-public (deactivate-creator (creator principal))
  (let ((creator-data (unwrap! (map-get? creators creator) (err err-not-found))))
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (map-set creators creator (merge creator-data {active: false}))
    (ok true)
  )
)