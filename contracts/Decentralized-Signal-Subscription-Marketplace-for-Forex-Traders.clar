(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROVIDER-EXISTS (err u101))
(define-constant ERR-PROVIDER-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-STAKE (err u103))
(define-constant ERR-SUBSCRIPTION-EXISTS (err u104))
(define-constant ERR-SUBSCRIPTION-NOT-FOUND (err u105))
(define-constant ERR-INVALID-SUBSCRIPTION (err u106))

(define-data-var min-stake-amount uint u1000)
(define-data-var subscription-fee uint u100)

(define-map providers
    principal
    {
        stake-amount: uint,
        total-subscribers: uint,
        performance-score: uint,
        active: bool,
        subscription-fee: uint
    }
)

(define-map subscriptions
    { subscriber: principal, provider: principal }
    {
        expires-at: uint,
        active: bool
    }
)

(define-map performance-history
    principal
    (list 20 {
        timestamp: uint,
        trade-result: int,
        verified: bool
    })
)

(define-public (register-provider (stake-amount uint) (custom-subscription-fee uint))
    (let ((provider tx-sender))
        (asserts! (>= stake-amount (var-get min-stake-amount)) ERR-INSUFFICIENT-STAKE)
        (asserts! (is-none (map-get? providers provider)) ERR-PROVIDER-EXISTS)

        (try! (stx-transfer? stake-amount provider (as-contract tx-sender)))

        (ok (map-set providers
            provider
            {
                stake-amount: stake-amount,
                total-subscribers: u0,
                performance-score: u0,
                active: true,
                subscription-fee: custom-subscription-fee
            }
        ))
    )
)

(define-public (update-subscription-fee (new-fee uint))
    (let ((provider tx-sender))
        (asserts! (is-some (map-get? providers provider)) ERR-PROVIDER-NOT-FOUND)
        (let ((current-data (unwrap-panic (map-get? providers provider))))
            (ok (map-set providers
                provider
                (merge current-data { subscription-fee: new-fee })
            ))
        )
    )
)

(define-public (update-performance (trade-result int))
    (let ((provider tx-sender))
        (asserts! (is-some (map-get? providers provider)) ERR-PROVIDER-NOT-FOUND)
        
        (ok (map-set performance-history
            provider
            (unwrap-panic (as-max-len? 
                (append (default-to (list) (map-get? performance-history provider))
                {
                    timestamp: burn-block-height,
                    trade-result: trade-result,
                    verified: true
                })
                u20))))
    )
)

(define-public (subscribe-to-provider (provider principal))
    (let (
        (subscriber tx-sender)
        (subscription-key { subscriber: subscriber, provider: provider })
        (provider-data (unwrap! (map-get? providers provider) ERR-PROVIDER-NOT-FOUND))
        (fee (get subscription-fee provider-data))
    )
        (asserts! (is-some (map-get? providers provider)) ERR-PROVIDER-NOT-FOUND)
        (asserts! (is-none (map-get? subscriptions subscription-key)) ERR-SUBSCRIPTION-EXISTS)

        (try! (stx-transfer? fee subscriber provider))

        (ok (map-set subscriptions
            subscription-key
            {
                expires-at: (+ burn-block-height u1440),
                active: true
            }))
    )
)

(define-public (cancel-subscription (provider principal))
    (let (
        (subscriber tx-sender)
        (subscription-key { subscriber: subscriber, provider: provider })
    )
        (asserts! (is-some (map-get? subscriptions subscription-key)) ERR-SUBSCRIPTION-NOT-FOUND)
        
        (ok (map-set subscriptions
            subscription-key
            {
                expires-at: burn-block-height,
                active: false
            }))
    )
)

(define-read-only (get-provider-details (provider principal))
    (ok (map-get? providers provider))
)

(define-read-only (get-subscription-details (subscriber principal) (provider principal))
    (ok (map-get? subscriptions { subscriber: subscriber, provider: provider }))
)

(define-read-only (get-performance-history (provider principal))
    (ok (map-get? performance-history provider))
)

(define-map provider-reputation
    principal
    {
        reputation-score: uint,
        total-profitable-trades: uint,
        total-trades: uint,
        last-updated: uint
    }
)

(define-private (calculate-reputation-score (performance-list (list 20 { timestamp: uint, trade-result: int, verified: bool })))
    (let (
        (total-trades (len performance-list))
        (profitable-trades (fold count-profitable-trades performance-list u0))
    )
        (if (> total-trades u0)
            (/ (* profitable-trades u100) total-trades)
            u0
        )
    )
)

(define-private (count-profitable-trades (trade { timestamp: uint, trade-result: int, verified: bool }) (acc uint))
    (if (and (get verified trade) (> (get trade-result trade) 0))
        (+ acc u1)
        acc
    )
)

(define-public (update-provider-reputation (provider-address principal))
    (let (
        (provider-data (unwrap! (map-get? providers provider-address) ERR-PROVIDER-NOT-FOUND))
        (performance-data (default-to (list) (map-get? performance-history provider-address)))
        (reputation-score (calculate-reputation-score performance-data))
        (total-trades (len performance-data))
        (profitable-trades (fold count-profitable-trades performance-data u0))
    )
        (map-set provider-reputation
            provider-address
            {
                reputation-score: reputation-score,
                total-profitable-trades: profitable-trades,
                total-trades: total-trades,
                last-updated: burn-block-height
            }
        )
        (ok reputation-score)
    )
)

(define-read-only (get-provider-reputation-score (provider-address principal))
    (ok (map-get? provider-reputation provider-address))
)

(define-read-only (is-top-performer (provider-address principal) (min-score uint))
    (let ((reputation-data (map-get? provider-reputation provider-address)))
        (match reputation-data
            data (ok (>= (get reputation-score data) min-score))
            (ok false)
        )
    )
)