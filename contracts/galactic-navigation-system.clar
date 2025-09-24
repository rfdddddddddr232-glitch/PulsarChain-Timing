;; PulsarChain-Timing: Galactic Navigation System Contract
;; Spacecraft navigation using pulsar timing signals as cosmic lighthouses

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_INVALID_SPACECRAFT (err u301))
(define-constant ERR_INVALID_POSITION (err u302))
(define-constant ERR_INSUFFICIENT_PULSARS (err u303))
(define-constant MIN_PULSARS_FOR_NAVIGATION u4)

;; Data Variables
(define-data-var next-spacecraft-id uint u1)
(define-data-var next-beacon-id uint u1)
(define-data-var navigation-active bool true)

;; Data Maps
(define-map spacecraft-registry uint {
    call-sign: (string-ascii 32),
    agency: (string-ascii 64),
    mission-type: (string-ascii 32),
    launch-date: uint,
    mass-kg: uint,
    operator: principal,
    navigation-capability: (string-ascii 32),
    pulsar-receiver-count: uint,
    last-position-update: uint,
    status: (string-ascii 16),
    authorization-level: uint
})

(define-map navigation-beacons uint {
    pulsar-name: (string-ascii 64),
    right-ascension: uint,
    declination: int,
    period-ns: uint,
    period-derivative: int,
    distance-kpc: uint,
    timing-precision: uint,
    signal-strength: uint,
    last-calibration: uint,
    reliability-score: uint,
    beacon-status: (string-ascii 16)
})

(define-map position-fixes uint {
    spacecraft-id: uint,
    timestamp: uint,
    position-x: int,
    position-y: int,
    position-z: int,
    velocity-x: int,
    velocity-y: int,
    velocity-z: int,
    position-uncertainty: uint,
    velocity-uncertainty: uint,
    pulsars-used: (list 10 uint),
    navigation-quality: uint,
    operator: principal
})

;; Private Functions
(define-private (is-valid-coordinates (x int) (y int) (z int))
    (and 
        (>= x -1000000000000)
        (<= x 1000000000000)
        (>= y -1000000000000)
        (<= y 1000000000000)
        (>= z -1000000000000)
        (<= z 1000000000000)
    )
)

(define-private (is-authorized-operator (operator principal))
    true
)

;; Public Functions
(define-public (register-spacecraft
    (call-sign (string-ascii 32))
    (agency (string-ascii 64))
    (mission-type (string-ascii 32))
    (mass-kg uint)
    (receiver-count uint)
    (nav-capability (string-ascii 32))
)
    (let ((spacecraft-id (var-get next-spacecraft-id)))
        (asserts! (var-get navigation-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-operator tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> (len call-sign) u0) ERR_INVALID_SPACECRAFT)
        (asserts! (> mass-kg u0) ERR_INVALID_SPACECRAFT)
        (asserts! (> receiver-count u0) ERR_INVALID_SPACECRAFT)
        
        (map-set spacecraft-registry spacecraft-id {
            call-sign: call-sign,
            agency: agency,
            mission-type: mission-type,
            launch-date: stacks-block-height,
            mass-kg: mass-kg,
            operator: tx-sender,
            navigation-capability: nav-capability,
            pulsar-receiver-count: receiver-count,
            last-position-update: u0,
            status: "registered",
            authorization-level: u1
        })
        
        (var-set next-spacecraft-id (+ spacecraft-id u1))
        (ok spacecraft-id)
    )
)

(define-public (register-navigation-beacon
    (pulsar-name (string-ascii 64))
    (ra uint)
    (dec int)
    (period uint)
    (period-dot int)
    (distance uint)
    (precision uint)
)
    (let ((beacon-id (var-get next-beacon-id)))
        (asserts! (var-get navigation-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-operator tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> (len pulsar-name) u0) ERR_INVALID_SPACECRAFT)
        (asserts! (and (<= ra u1296000000) (>= dec -324000000) (<= dec 324000000)) ERR_INVALID_POSITION)
        (asserts! (> period u1000000) ERR_INVALID_POSITION)
        
        (map-set navigation-beacons beacon-id {
            pulsar-name: pulsar-name,
            right-ascension: ra,
            declination: dec,
            period-ns: period,
            period-derivative: period-dot,
            distance-kpc: distance,
            timing-precision: precision,
            signal-strength: u1000,
            last-calibration: stacks-block-height,
            reliability-score: u80,
            beacon-status: "active"
        })
        
        (var-set next-beacon-id (+ beacon-id u1))
        (ok beacon-id)
    )
)

(define-public (submit-position-fix
    (spacecraft-id uint)
    (position {x: int, y: int, z: int})
    (velocity {x: int, y: int, z: int})
    (pulsars-used (list 10 uint))
)
    (begin
        (asserts! (var-get navigation-active) ERR_UNAUTHORIZED)
        (asserts! (is-authorized-operator tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? spacecraft-registry spacecraft-id)) ERR_INVALID_SPACECRAFT)
        (asserts! (is-valid-coordinates (get x position) (get y position) (get z position)) ERR_INVALID_POSITION)
        (asserts! (>= (len pulsars-used) MIN_PULSARS_FOR_NAVIGATION) ERR_INSUFFICIENT_PULSARS)
        
        (let (
            (fix-id (+ (* spacecraft-id u1000000) stacks-block-height))
        )
            (map-set position-fixes fix-id {
                spacecraft-id: spacecraft-id,
                timestamp: stacks-block-height,
                position-x: (get x position),
                position-y: (get y position),
                position-z: (get z position),
                velocity-x: (get x velocity),
                velocity-y: (get y velocity),
                velocity-z: (get z velocity),
                position-uncertainty: u1000,
                velocity-uncertainty: u1000,
                pulsars-used: pulsars-used,
                navigation-quality: u90,
                operator: tx-sender
            })
            
            (ok fix-id)
        )
    )
)

;; Read-only functions
(define-read-only (get-spacecraft-info (spacecraft-id uint))
    (map-get? spacecraft-registry spacecraft-id)
)

(define-read-only (get-navigation-beacon (beacon-id uint))
    (map-get? navigation-beacons beacon-id)
)

(define-read-only (get-position-fix (fix-id uint))
    (map-get? position-fixes fix-id)
)

(define-read-only (get-navigation-stats)
    {
        total-spacecraft: (var-get next-spacecraft-id),
        total-beacons: (var-get next-beacon-id),
        navigation-active: (var-get navigation-active)
    }
)

;; Admin functions
(define-public (toggle-navigation-system)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set navigation-active (not (var-get navigation-active)))
        (ok (var-get navigation-active))
    )
)
