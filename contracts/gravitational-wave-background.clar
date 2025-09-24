;; PulsarChain-Timing: Gravitational Wave Background Detection Contract
;; Pulsar timing array analysis detecting low-frequency gravitational wave signatures

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INVALID_ANALYSIS (err u201))
(define-constant ERR_INSUFFICIENT_DATA (err u202))
(define-constant ERR_INVALID_FREQUENCY_RANGE (err u203))
(define-constant ERR_DETECTION_EXISTS (err u204))
(define-constant ERR_INVALID_AMPLITUDE (err u205))

;; Gravitational wave constants
(define-constant MIN_FREQUENCY_NANO_HZ u1)
(define-constant MAX_FREQUENCY_NANO_HZ u1000000000)
(define-constant MIN_STRAIN_AMPLITUDE u1)
(define-constant DETECTION_CONFIDENCE_THRESHOLD u95)
(define-constant MIN_PULSARS_FOR_DETECTION u5)

;; Data Variables
(define-data-var next-analysis-id uint u1)
(define-data-var next-detection-id uint u1)
(define-data-var next-array-id uint u1)
(define-data-var contract-active bool true)
(define-data-var global-detection-threshold uint u90)

;; Data Maps
(define-map pulsar-timing-arrays uint {
    name: (string-ascii 64),
    coordinator: principal,
    pulsar-count: uint,
    observatories: (list 20 (string-ascii 32)),
    frequency-range-min: uint,
    frequency-range-max: uint,
    sensitivity: uint,
    created-at: uint,
    status: (string-ascii 16),
    total-observations: uint,
    last-analysis: uint
})

(define-map gravitational-wave-analyses uint {
    array-id: uint,
    analyst: principal,
    analysis-type: (string-ascii 32),
    frequency-bin-start: uint,
    frequency-bin-end: uint,
    time-span-blocks: uint,
    data-points: uint,
    cross-correlation-matrix: (list 25 int),
    noise-level: uint,
    signal-strength: uint,
    confidence-score: uint,
    analysis-timestamp: uint,
    validation-status: (string-ascii 16)
})

(define-map gw-detection-candidates uint {
    analysis-id: uint,
    detection-type: (string-ascii 32),
    source-candidate: (string-ascii 64),
    frequency-hz: uint,
    strain-amplitude: uint,
    sky-location-ra: uint,
    sky-location-dec: int,
    detection-confidence: uint,
    discovery-timestamp: uint,
    discoverer: principal,
    verification-count: uint,
    false-alarm-probability: uint,
    status: (string-ascii 16)
})

;; Private Functions
(define-private (is-valid-frequency-range (min-freq uint) (max-freq uint))
    (and 
        (>= min-freq MIN_FREQUENCY_NANO_HZ)
        (<= max-freq MAX_FREQUENCY_NANO_HZ)
        (< min-freq max-freq)
    )
)

(define-private (calculate-detection-significance (signal uint) (noise uint))
    (if (> noise u0)
        (/ (* signal u100) noise)
        u0
    )
)

(define-private (is-authorized-analyst (analyst principal))
    true
)

(define-private (validate-strain-amplitude (amplitude uint))
    (and 
        (>= amplitude MIN_STRAIN_AMPLITUDE)
        (<= amplitude u1000000000000000000)
    )
)

;; Public Functions
(define-public (create-timing-array
    (name (string-ascii 64))
    (observatories (list 20 (string-ascii 32)))
    (freq-min uint)
    (freq-max uint)
    (target-sensitivity uint)
)
    (let ((array-id (var-get next-array-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-analyst tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-valid-frequency-range freq-min freq-max) ERR_INVALID_FREQUENCY_RANGE)
        (asserts! (validate-strain-amplitude target-sensitivity) ERR_INVALID_AMPLITUDE)
        (asserts! (> (len observatories) u0) ERR_INSUFFICIENT_DATA)
        (asserts! (> (len name) u0) ERR_INVALID_ANALYSIS)
        
        (map-set pulsar-timing-arrays array-id {
            name: name,
            coordinator: tx-sender,
            pulsar-count: u0,
            observatories: observatories,
            frequency-range-min: freq-min,
            frequency-range-max: freq-max,
            sensitivity: target-sensitivity,
            created-at: stacks-block-height,
            status: "active",
            total-observations: u0,
            last-analysis: u0
        })
        
        (var-set next-array-id (+ array-id u1))
        (ok array-id)
    )
)

(define-public (submit-gw-analysis
    (array-id uint)
    (analysis-type (string-ascii 32))
    (freq-start uint)
    (freq-end uint)
    (time-span uint)
    (data-points uint)
    (cross-correlations (list 25 int))
    (noise-level uint)
    (signal-strength uint)
)
    (let ((analysis-id (var-get next-analysis-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-analyst tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? pulsar-timing-arrays array-id)) ERR_INVALID_ANALYSIS)
        (asserts! (is-valid-frequency-range freq-start freq-end) ERR_INVALID_FREQUENCY_RANGE)
        (asserts! (> data-points u10) ERR_INSUFFICIENT_DATA)
        (asserts! (> time-span u0) ERR_INVALID_ANALYSIS)
        
        (let ((confidence (calculate-detection-significance signal-strength noise-level)))
            (map-set gravitational-wave-analyses analysis-id {
                array-id: array-id,
                analyst: tx-sender,
                analysis-type: analysis-type,
                frequency-bin-start: freq-start,
                frequency-bin-end: freq-end,
                time-span-blocks: time-span,
                data-points: data-points,
                cross-correlation-matrix: cross-correlations,
                noise-level: noise-level,
                signal-strength: signal-strength,
                confidence-score: confidence,
                analysis-timestamp: stacks-block-height,
                validation-status: "pending"
            })
            
            (var-set next-analysis-id (+ analysis-id u1))
            (ok analysis-id)
        )
    )
)

(define-public (report-gw-detection
    (analysis-id uint)
    (detection-type (string-ascii 32))
    (source-name (string-ascii 64))
    (frequency uint)
    (strain-amplitude uint)
    (ra uint)
    (dec int)
)
    (let ((detection-id (var-get next-detection-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-analyst tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? gravitational-wave-analyses analysis-id)) ERR_INVALID_ANALYSIS)
        (asserts! (validate-strain-amplitude strain-amplitude) ERR_INVALID_AMPLITUDE)
        (asserts! (and (>= frequency MIN_FREQUENCY_NANO_HZ) (<= frequency MAX_FREQUENCY_NANO_HZ)) ERR_INVALID_FREQUENCY_RANGE)
        
        (let (
            (analysis (unwrap! (map-get? gravitational-wave-analyses analysis-id) ERR_INVALID_ANALYSIS))
            (detection-confidence (get confidence-score analysis))
        )
            (asserts! (>= detection-confidence (var-get global-detection-threshold)) ERR_INVALID_ANALYSIS)
            
            (map-set gw-detection-candidates detection-id {
                analysis-id: analysis-id,
                detection-type: detection-type,
                source-candidate: source-name,
                frequency-hz: frequency,
                strain-amplitude: strain-amplitude,
                sky-location-ra: ra,
                sky-location-dec: dec,
                detection-confidence: detection-confidence,
                discovery-timestamp: stacks-block-height,
                discoverer: tx-sender,
                verification-count: u0,
                false-alarm-probability: (- u100 detection-confidence),
                status: "candidate"
            })
            
            (var-set next-detection-id (+ detection-id u1))
            (ok detection-id)
        )
    )
)

;; Read-only functions
(define-read-only (get-timing-array (array-id uint))
    (map-get? pulsar-timing-arrays array-id)
)

(define-read-only (get-gw-analysis (analysis-id uint))
    (map-get? gravitational-wave-analyses analysis-id)
)

(define-read-only (get-detection-candidate (detection-id uint))
    (map-get? gw-detection-candidates detection-id)
)

(define-read-only (get-contract-status)
    {
        next-analysis-id: (var-get next-analysis-id),
        next-detection-id: (var-get next-detection-id),
        next-array-id: (var-get next-array-id),
        active: (var-get contract-active),
        detection-threshold: (var-get global-detection-threshold)
    }
)

;; Admin functions
(define-public (set-detection-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-threshold u100) ERR_INVALID_ANALYSIS)
        (var-set global-detection-threshold new-threshold)
        (ok true)
    )
)

(define-public (toggle-contract-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-active (not (var-get contract-active)))
        (ok (var-get contract-active))
    )
)
