;; PulsarChain-Timing: Pulsar Timing Precision Contract
;; Radio telescope networks monitoring pulsar rotation timing with nanosecond accuracy
;; Manages precision timing measurements and validation for neutron star observations

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_PULSAR (err u101))
(define-constant ERR_INVALID_TIMING (err u102))
(define-constant ERR_INSUFFICIENT_PRECISION (err u103))

;; Precision constants (nanoseconds)
(define-constant MIN_TIMING_PRECISION u1000)
(define-constant MAX_ROTATION_PERIOD u10000000000)
(define-constant MIN_ROTATION_PERIOD u1000000)

;; Data Variables
(define-data-var next-pulsar-id uint u1)
(define-data-var next-timing-id uint u1)
(define-data-var contract-active bool true)
(define-data-var timing-precision-threshold uint u100)

;; Data Maps
(define-map pulsars uint {
    name: (string-ascii 64),
    right-ascension: uint,
    declination: int,
    period: uint,
    period-derivative: int,
    discovery-date: uint,
    observer: principal,
    status: (string-ascii 16),
    observation-count: uint,
    last-observation: uint
})

(define-map timing-measurements uint {
    pulsar-id: uint,
    observatory: (string-ascii 64),
    observer: principal,
    timestamp: uint,
    arrival-time: uint,
    frequency: uint,
    pulse-number: uint,
    timing-residual: int,
    uncertainty: uint,
    weather-conditions: (string-ascii 32),
    equipment-id: (string-ascii 32),
    validation-status: (string-ascii 16)
})

(define-map observatory-credentials principal {
    name: (string-ascii 64),
    location: (string-ascii 64),
    equipment-type: (string-ascii 32),
    precision-rating: uint,
    certification-date: uint,
    active: bool,
    observations-count: uint,
    reputation-score: uint
})

;; Private Functions
(define-private (is-valid-coordinates (ra uint) (dec int))
    (and 
        (<= ra u1296000000)
        (>= dec -324000000)
        (<= dec 324000000)
    )
)

(define-private (is-valid-period (period uint))
    (and 
        (>= period MIN_ROTATION_PERIOD)
        (<= period MAX_ROTATION_PERIOD)
    )
)

(define-private (is-authorized-observer (observer principal))
    (is-some (map-get? observatory-credentials observer))
)

(define-private (validate-timing-data (residual int) (uncertainty uint) (frequency uint))
    (and 
        (<= uncertainty (* (var-get timing-precision-threshold) u10))
        (> frequency u100000)
        (< frequency u10000000)
    )
)

;; Public Functions
(define-public (register-pulsar 
    (name (string-ascii 64))
    (right-ascension uint)
    (declination int)
    (period uint)
    (period-derivative int)
)
    (let ((pulsar-id (var-get next-pulsar-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-observer tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-valid-coordinates right-ascension declination) ERR_INVALID_PULSAR)
        (asserts! (is-valid-period period) ERR_INVALID_PULSAR)
        (asserts! (> (len name) u0) ERR_INVALID_PULSAR)
        
        (map-set pulsars pulsar-id {
            name: name,
            right-ascension: right-ascension,
            declination: declination,
            period: period,
            period-derivative: period-derivative,
            discovery-date: stacks-block-height,
            observer: tx-sender,
            status: "active",
            observation-count: u0,
            last-observation: u0
        })
        
        (var-set next-pulsar-id (+ pulsar-id u1))
        (ok pulsar-id)
    )
)

(define-public (submit-timing-measurement
    (pulsar-id uint)
    (observatory (string-ascii 64))
    (arrival-time uint)
    (frequency uint)
    (pulse-number uint)
    (timing-residual int)
    (uncertainty uint)
    (weather-conditions (string-ascii 32))
    (equipment-id (string-ascii 32))
)
    (let ((timing-id (var-get next-timing-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-observer tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? pulsars pulsar-id)) ERR_INVALID_PULSAR)
        (asserts! (validate-timing-data timing-residual uncertainty frequency) ERR_INVALID_TIMING)
        (asserts! (<= uncertainty MIN_TIMING_PRECISION) ERR_INSUFFICIENT_PRECISION)
        
        (map-set timing-measurements timing-id {
            pulsar-id: pulsar-id,
            observatory: observatory,
            observer: tx-sender,
            timestamp: stacks-block-height,
            arrival-time: arrival-time,
            frequency: frequency,
            pulse-number: pulse-number,
            timing-residual: timing-residual,
            uncertainty: uncertainty,
            weather-conditions: weather-conditions,
            equipment-id: equipment-id,
            validation-status: "pending"
        })
        
        (var-set next-timing-id (+ timing-id u1))
        (ok timing-id)
    )
)

(define-public (register-observatory
    (name (string-ascii 64))
    (location (string-ascii 64))
    (equipment-type (string-ascii 32))
    (precision-rating uint)
)
    (begin
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? observatory-credentials tx-sender)) ERR_UNAUTHORIZED)
        (asserts! (> (len name) u0) ERR_INVALID_PULSAR)
        (asserts! (<= precision-rating MIN_TIMING_PRECISION) ERR_INSUFFICIENT_PRECISION)
        
        (map-set observatory-credentials tx-sender {
            name: name,
            location: location,
            equipment-type: equipment-type,
            precision-rating: precision-rating,
            certification-date: stacks-block-height,
            active: true,
            observations-count: u0,
            reputation-score: u100
        })
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-pulsar (pulsar-id uint))
    (map-get? pulsars pulsar-id)
)

(define-read-only (get-timing-measurement (timing-id uint))
    (map-get? timing-measurements timing-id)
)

(define-read-only (get-observatory-info (observer principal))
    (map-get? observatory-credentials observer)
)

(define-read-only (get-contract-stats)
    {
        next-pulsar-id: (var-get next-pulsar-id),
        next-timing-id: (var-get next-timing-id),
        active: (var-get contract-active),
        precision-threshold: (var-get timing-precision-threshold)
    }
)

;; Admin functions
(define-public (set-contract-status (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-active active)
        (ok true)
    )
)

(define-public (update-precision-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-threshold u0) ERR_INVALID_TIMING)
        (var-set timing-precision-threshold new-threshold)
        (ok true)
    )
)
