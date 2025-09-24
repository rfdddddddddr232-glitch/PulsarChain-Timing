;; PulsarChain-Timing: Pulsar Astronomy Rewards Contract
;; Token incentives for pulsar observation, timing analysis, and deep space navigation research

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_INSUFFICIENT_BALANCE (err u401))
(define-constant ERR_INVALID_REWARD_AMOUNT (err u402))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u403))
(define-constant ERR_INVALID_CONTRIBUTION (err u404))
(define-constant ERR_POOL_NOT_FOUND (err u405))

;; Token and reward constants
(define-constant REWARD_TOKEN_NAME "PulsarChain Token")
(define-constant REWARD_TOKEN_SYMBOL "PCT")
(define-constant TOTAL_SUPPLY u100000000000000)
(define-constant DECIMALS u6)
(define-constant BASE_OBSERVATION_REWARD u1000000)
(define-constant BASE_TIMING_ANALYSIS_REWARD u5000000)
(define-constant BASE_NAVIGATION_REWARD u10000000)
(define-constant BASE_DISCOVERY_REWARD u50000000)
(define-constant MIN_STAKE_AMOUNT u1000000)

;; Token implementation
(define-fungible-token pulsar-chain-token TOTAL_SUPPLY)

;; Data Variables
(define-data-var next-reward-pool-id uint u1)
(define-data-var next-contribution-id uint u1)
(define-data-var contract-active bool true)
(define-data-var total-rewards-distributed uint u0)
(define-data-var governance-treasury uint u0)
(define-data-var research-fund-balance uint u0)
(define-data-var reward-multiplier uint u100)

;; Data Maps
(define-map reward-pools uint {
    pool-name: (string-ascii 64),
    pool-type: (string-ascii 32),
    total-allocation: uint,
    distributed-amount: uint,
    reward-rate: uint,
    multiplier-active: bool,
    quality-threshold: uint,
    pool-manager: principal,
    created-at: uint,
    expires-at: uint,
    active: bool
})

(define-map research-contributions uint {
    contributor: principal,
    contribution-type: (string-ascii 32),
    research-category: (string-ascii 32),
    quality-score: uint,
    data-points: uint,
    verification-count: uint,
    peer-review-score: uint,
    impact-factor: uint,
    submission-time: uint,
    verification-deadline: uint,
    verified: bool,
    reward-pool-id: uint
})

(define-map reward-claims uint {
    contribution-id: uint,
    claimant: principal,
    reward-amount: uint,
    bonus-multiplier: uint,
    claim-timestamp: uint,
    pool-id: uint,
    processing-status: (string-ascii 16)
})

(define-map staking-positions principal {
    staked-amount: uint,
    stake-timestamp: uint,
    lock-duration: uint,
    reward-multiplier: uint,
    accumulated-rewards: uint,
    last-reward-claim: uint,
    auto-compound: bool
})

(define-map reputation-scores principal {
    total-contributions: uint,
    quality-average: uint,
    peer-endorsements: uint,
    successful-discoveries: uint,
    reputation-level: uint,
    bonus-eligibility: bool,
    last-updated: uint
})

;; Private Functions
(define-private (calculate-quality-bonus (quality-score uint))
    (if (>= quality-score u90)
        u150
        (if (>= quality-score u75)
            u125
            u100
        )
    )
)

(define-private (validate-contribution-quality (quality-score uint) (data-points uint) (verification-count uint))
    (and 
        (<= quality-score u100)
        (> data-points u0)
        (or (>= verification-count u2) (<= quality-score u50))
    )
)

;; Token functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
        (ft-transfer? pulsar-chain-token amount sender recipient)
    )
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance pulsar-chain-token who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply pulsar-chain-token))
)

(define-read-only (get-name)
    (ok REWARD_TOKEN_NAME)
)

(define-read-only (get-symbol)
    (ok REWARD_TOKEN_SYMBOL)
)

(define-read-only (get-decimals)
    (ok DECIMALS)
)

;; Public Functions
(define-public (create-reward-pool
    (pool-name (string-ascii 64))
    (pool-type (string-ascii 32))
    (total-allocation uint)
    (reward-rate uint)
    (quality-threshold uint)
    (duration-blocks uint)
)
    (let ((pool-id (var-get next-reward-pool-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> total-allocation u0) ERR_INVALID_REWARD_AMOUNT)
        (asserts! (> reward-rate u0) ERR_INVALID_REWARD_AMOUNT)
        (asserts! (<= quality-threshold u100) ERR_INVALID_CONTRIBUTION)
        
        (try! (ft-mint? pulsar-chain-token total-allocation (as-contract tx-sender)))
        
        (map-set reward-pools pool-id {
            pool-name: pool-name,
            pool-type: pool-type,
            total-allocation: total-allocation,
            distributed-amount: u0,
            reward-rate: reward-rate,
            multiplier-active: true,
            quality-threshold: quality-threshold,
            pool-manager: tx-sender,
            created-at: stacks-block-height,
            expires-at: (+ stacks-block-height duration-blocks),
            active: true
        })
        
        (var-set next-reward-pool-id (+ pool-id u1))
        (ok pool-id)
    )
)

(define-public (submit-contribution
    (contribution-type (string-ascii 32))
    (research-category (string-ascii 32))
    (data-points uint)
    (self-assessed-quality uint)
    (pool-id uint)
)
    (let ((contribution-id (var-get next-contribution-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? reward-pools pool-id)) ERR_POOL_NOT_FOUND)
        (asserts! (validate-contribution-quality self-assessed-quality data-points u0) ERR_INVALID_CONTRIBUTION)
        
        (map-set research-contributions contribution-id {
            contributor: tx-sender,
            contribution-type: contribution-type,
            research-category: research-category,
            quality-score: self-assessed-quality,
            data-points: data-points,
            verification-count: u0,
            peer-review-score: u0,
            impact-factor: u0,
            submission-time: stacks-block-height,
            verification-deadline: (+ stacks-block-height u1008),
            verified: false,
            reward-pool-id: pool-id
        })
        
        (var-set next-contribution-id (+ contribution-id u1))
        (ok contribution-id)
    )
)

(define-public (verify-contribution
    (contribution-id uint)
    (quality-assessment uint)
    (peer-review-score uint)
    (impact-factor uint)
)
    (let ((contribution (unwrap! (map-get? research-contributions contribution-id) ERR_INVALID_CONTRIBUTION)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq tx-sender (get contributor contribution))) ERR_UNAUTHORIZED)
        (asserts! (<= quality-assessment u100) ERR_INVALID_CONTRIBUTION)
        (asserts! (<= peer-review-score u100) ERR_INVALID_CONTRIBUTION)
        
        (map-set research-contributions contribution-id 
            (merge contribution {
                verification-count: (+ (get verification-count contribution) u1),
                peer-review-score: (/ (+ (get peer-review-score contribution) peer-review-score) u2),
                impact-factor: impact-factor,
                verified: (>= (+ (get verification-count contribution) u1) u2),
                quality-score: (/ (+ (get quality-score contribution) quality-assessment) u2)
            })
        )
        
        (ok true)
    )
)

(define-public (claim-contribution-reward (contribution-id uint))
    (let ((contribution (unwrap! (map-get? research-contributions contribution-id) ERR_INVALID_CONTRIBUTION)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq tx-sender (get contributor contribution)) ERR_UNAUTHORIZED)
        (asserts! (get verified contribution) ERR_INVALID_CONTRIBUTION)
        
        (let (
            (pool-id (get reward-pool-id contribution))
            (pool (unwrap! (map-get? reward-pools pool-id) ERR_POOL_NOT_FOUND))
            (base-reward (get reward-rate pool))
            (quality-bonus (calculate-quality-bonus (get quality-score contribution)))
            (total-reward (/ (* base-reward quality-bonus) u100))
        )
            (asserts! (>= (get quality-score contribution) (get quality-threshold pool)) ERR_INVALID_CONTRIBUTION)
            (asserts! (<= total-reward (- (get total-allocation pool) (get distributed-amount pool))) ERR_INSUFFICIENT_BALANCE)
            
            (try! (as-contract (ft-transfer? pulsar-chain-token total-reward (as-contract tx-sender) tx-sender)))
            
            (map-set reward-pools pool-id 
                (merge pool {
                    distributed-amount: (+ (get distributed-amount pool) total-reward)
                })
            )
            
            (map-set reward-claims contribution-id {
                contribution-id: contribution-id,
                claimant: tx-sender,
                reward-amount: total-reward,
                bonus-multiplier: quality-bonus,
                claim-timestamp: stacks-block-height,
                pool-id: pool-id,
                processing-status: "completed"
            })
            
            (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) total-reward))
            (ok total-reward)
        )
    )
)

(define-public (stake-tokens (amount uint) (lock-duration uint))
    (begin
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (>= amount MIN_STAKE_AMOUNT) ERR_INVALID_REWARD_AMOUNT)
        (asserts! (>= lock-duration u1008) ERR_INVALID_CONTRIBUTION)
        
        (try! (ft-transfer? pulsar-chain-token amount tx-sender (as-contract tx-sender)))
        
        (let ((multiplier (+ u100 (/ lock-duration u1008))))
            (map-set staking-positions tx-sender {
                staked-amount: amount,
                stake-timestamp: stacks-block-height,
                lock-duration: lock-duration,
                reward-multiplier: multiplier,
                accumulated-rewards: u0,
                last-reward-claim: stacks-block-height,
                auto-compound: false
            })
            (ok amount)
        )
    )
)

;; Read-only functions
(define-read-only (get-reward-pool (pool-id uint))
    (map-get? reward-pools pool-id)
)

(define-read-only (get-contribution (contribution-id uint))
    (map-get? research-contributions contribution-id)
)

(define-read-only (get-reward-claim (contribution-id uint))
    (map-get? reward-claims contribution-id)
)

(define-read-only (get-staking-position (staker principal))
    (map-get? staking-positions staker)
)

(define-read-only (get-reputation-score (contributor principal))
    (map-get? reputation-scores contributor)
)

(define-read-only (get-contract-stats)
    {
        total-pools: (var-get next-reward-pool-id),
        total-contributions: (var-get next-contribution-id),
        total-rewards-distributed: (var-get total-rewards-distributed),
        research-fund-balance: (var-get research-fund-balance),
        contract-active: (var-get contract-active),
        reward-multiplier: (var-get reward-multiplier)
    }
)

;; Admin functions
(define-public (toggle-contract-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-active (not (var-get contract-active)))
        (ok (var-get contract-active))
    )
)

(define-public (update-reward-multiplier (new-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (and (>= new-multiplier u50) (<= new-multiplier u200)) ERR_INVALID_REWARD_AMOUNT)
        (var-set reward-multiplier new-multiplier)
        (ok true)
    )
)
