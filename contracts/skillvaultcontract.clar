;; title: SkillVault - Competency-Backed Lending Protocol
;; version: 1.0.0
;; summary: A decentralized lending platform that uses verified skills and competencies as collateral
;; description: SkillVault enables loans backed by human capital through skill verification oracles,
;;              dynamic credit scoring, peer vouching, and automated income-share agreements

;; traits
(define-trait skill-oracle-trait
  (
    (verify-skill (principal (string-ascii 50) uint) (response bool uint))
    (get-skill-score (principal (string-ascii 50)) (response uint uint))
  )
)

;; token definitions
(define-fungible-token skill-token)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-input (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-loan-not-active (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-skill-verification-failed (err u106))
(define-constant err-insufficient-reputation (err u107))
(define-constant err-payment-failed (err u108))
(define-constant err-overflow (err u109))
(define-constant err-insufficient-funds (err u110))
(define-constant err-loan-defaulted (err u111))
(define-constant err-invalid-term (err u112))

(define-constant max-loan-amount u1000000) ;; 1M microSTX
(define-constant min-reputation-score u100)
(define-constant skill-decay-rate u5) ;; 5% per period
(define-constant base-interest-rate u500) ;; 5% in basis points
(define-constant max-term-blocks u52560) ;; ~1 year
(define-constant max-skill-score u10000) ;; Maximum total skill score

;; data vars
(define-data-var next-loan-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points
(define-data-var total-loans-issued uint u0)
(define-data-var total-repaid uint u0)
(define-data-var paused bool false)

;; data maps
(define-map user-profiles
  { user: principal }
  {
    reputation-score: uint,
    total-loans: uint,
    successful-repayments: uint,
    skill-score: uint,
    last-verification: uint,
    is-active: bool
  }
)

(define-map skills
  { user: principal, skill-name: (string-ascii 50) }
  {
    score: uint,
    verified: bool,
    verifier: principal,
    verification-date: uint,
    decay-factor: uint
  }
)

(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    amount: uint,
    interest-rate: uint,
    term-blocks: uint,
    collateral-skills: (list 5 (string-ascii 50)),
    status: (string-ascii 20),
    created-at: uint,
    due-at: uint,
    repaid-amount: uint,
    income-share-rate: uint
  }
)

(define-map vouches
  { voucher: principal, borrower: principal }
  {
    amount: uint,
    skill-endorsed: (string-ascii 50),
    stake-amount: uint,
    active: bool
  }
)

(define-map skill-oracles
  { oracle: principal }
  {
    active: bool,
    skills-supported: (list 10 (string-ascii 50)),
    reputation: uint
  }
)

(define-map income-shares
  { loan-id: uint, payment-period: uint }
  {
    amount-due: uint,
    paid: bool,
    due-block: uint
  }
)

;; public functions

;; Initialize user profile
(define-public (create-profile)
  (let ((user tx-sender))
    (asserts! (is-none (map-get? user-profiles { user: user })) err-already-exists)
    (map-set user-profiles 
      { user: user }
      {
        reputation-score: u50,
        total-loans: u0,
        successful-repayments: u0,
        skill-score: u0,
        last-verification: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Register as skill oracle (only contract owner can register oracles)
(define-public (register-oracle (oracle principal) (skills-supported (list 10 (string-ascii 50))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (is-none (map-get? skill-oracles { oracle: oracle })) err-already-exists)
    (map-set skill-oracles
      { oracle: oracle }
      {
        active: true,
        skills-supported: skills-supported,
        reputation: u100
      }
    )
    (ok true)
  )
)

;; Add/verify skill through oracle (only oracle can verify)
(define-public (verify-skill (user principal) (skill-name (string-ascii 50)) (score uint))
  (let (
    (oracle tx-sender)
    (oracle-info (unwrap! (map-get? skill-oracles { oracle: oracle }) err-unauthorized))
    (profile (unwrap! (map-get? user-profiles { user: user }) err-not-found))
    (current-skill-score (get skill-score profile))
  )
    (asserts! (get active oracle-info) err-unauthorized)
    (asserts! (<= score u1000) err-invalid-input)
    (asserts! (not (var-get paused)) err-unauthorized)
    
    ;; Check for overflow before adding
    (asserts! (<= (+ current-skill-score score) max-skill-score) err-overflow)
    
    (map-set skills
      { user: user, skill-name: skill-name }
      {
        score: score,
        verified: true,
        verifier: oracle,
        verification-date: stacks-block-height,
        decay-factor: u100
      }
    )
    
    ;; Update user's total skill score with overflow protection
    (map-set user-profiles
      { user: user }
      (merge profile { 
        skill-score: (+ current-skill-score score),
        last-verification: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Peer vouching system
(define-public (vouch-for-user (borrower principal) (skill-name (string-ascii 50)) (stake-amount uint))
  (let ((voucher tx-sender))
    (asserts! (> stake-amount u0) err-invalid-input)
    (asserts! (not (is-eq voucher borrower)) err-invalid-input)
    (asserts! (not (var-get paused)) err-unauthorized)
    (asserts! (is-none (map-get? vouches { voucher: voucher, borrower: borrower })) err-already-exists)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? stake-amount voucher (as-contract tx-sender)))
    
    (map-set vouches
      { voucher: voucher, borrower: borrower }
      {
        amount: stake-amount,
        skill-endorsed: skill-name,
        stake-amount: stake-amount,
        active: true
      }
    )
    (ok true)
  )
)

;; Withdraw vouch stake
(define-public (withdraw-vouch (borrower principal))
  (let (
    (voucher tx-sender)
    (vouch-info (unwrap! (map-get? vouches { voucher: voucher, borrower: borrower }) err-not-found))
  )
    (asserts! (get active vouch-info) err-unauthorized)
    
    ;; Return stake to voucher
    (try! (as-contract (stx-transfer? (get stake-amount vouch-info) tx-sender voucher)))
    
    ;; Mark vouch as inactive
    (map-set vouches
      { voucher: voucher, borrower: borrower }
      (merge vouch-info { active: false })
    )
    (ok true)
  )
)

;; Calculate dynamic credit score
(define-private (calculate-credit-score (user principal))
  (let (
    (profile (unwrap! (map-get? user-profiles { user: user }) err-not-found))
    (skill-score (get skill-score profile))
    (reputation-score (get reputation-score profile))
    (success-rate (if (> (get total-loans profile) u0)
                    (/ (* (get successful-repayments profile) u100) (get total-loans profile))
                    u0))
  )
    (ok (+ skill-score (* reputation-score u2) success-rate))
  )
)

;; Request loan with security checks
(define-public (request-loan (amount uint) (term-blocks uint) (collateral-skills (list 5 (string-ascii 50))) (income-share-rate uint))
  (let (
    (borrower tx-sender)
    (loan-id (var-get next-loan-id))
    (credit-score (try! (calculate-credit-score borrower)))
    (interest-rate (calculate-interest-rate credit-score))
    (profile (unwrap! (map-get? user-profiles { user: borrower }) err-not-found))
    (due-block (+ stacks-block-height term-blocks))
  )
    ;; Input validation
    (asserts! (not (var-get paused)) err-unauthorized)
    (asserts! (> amount u0) err-invalid-input)
    (asserts! (<= amount max-loan-amount) err-invalid-input)
    (asserts! (> term-blocks u0) err-invalid-input)
    (asserts! (<= term-blocks max-term-blocks) err-invalid-term)
    (asserts! (>= credit-score min-reputation-score) err-insufficient-reputation)
    (asserts! (<= income-share-rate u2000) err-invalid-input) ;; Max 20%
    
    ;; Check contract has sufficient balance
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) err-insufficient-funds)
    
    ;; Update borrower profile BEFORE transfer (checks-effects-interactions)
    (map-set user-profiles
      { user: borrower }
      (merge profile { total-loans: (+ (get total-loans profile) u1) })
    )
    
    ;; Create loan record
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: borrower,
        amount: amount,
        interest-rate: interest-rate,
        term-blocks: term-blocks,
        collateral-skills: collateral-skills,
        status: "active",
        created-at: stacks-block-height,
        due-at: due-block,
        repaid-amount: u0,
        income-share-rate: income-share-rate
      }
    )
    
    ;; Update counters
    (var-set next-loan-id (+ loan-id u1))
    (var-set total-loans-issued (+ (var-get total-loans-issued) amount))
    
    ;; Transfer loan amount to borrower (last step)
    (try! (as-contract (stx-transfer? amount tx-sender borrower)))
    
    (ok loan-id)
  )
)

;; Make income share payment
(define-public (make-income-payment (loan-id uint) (payment-amount uint))
  (let (
    (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
    (borrower (get borrower loan))
    (profile (unwrap! (map-get? user-profiles { user: borrower }) err-not-found))
    (new-repaid-amount (+ (get repaid-amount loan) payment-amount))
    (total-due (+ (get amount loan) (/ (* (get amount loan) (get interest-rate loan)) u10000)))
  )
    (asserts! (is-eq tx-sender borrower) err-unauthorized)
    (asserts! (is-eq (get status loan) "active") err-loan-not-active)
    (asserts! (> payment-amount u0) err-invalid-input)
    
    ;; Transfer payment to contract
    (try! (stx-transfer? payment-amount borrower (as-contract tx-sender)))
    
    ;; Update loan repaid amount
    (map-set loans
      { loan-id: loan-id }
      (merge loan { repaid-amount: new-repaid-amount })
    )
    
    ;; Check if loan is fully repaid
    (if (>= new-repaid-amount total-due)
      (begin
        (map-set loans { loan-id: loan-id } (merge loan { 
          status: "repaid",
          repaid-amount: new-repaid-amount
        }))
        (var-set total-repaid (+ (var-get total-repaid) total-due))
        
        ;; Update borrower reputation
        (map-set user-profiles
          { user: borrower }
          (merge profile { 
            successful-repayments: (+ (get successful-repayments profile) u1),
            reputation-score: (if (> (+ (get reputation-score profile) u10) u1000)
                                  u1000
                                  (+ (get reputation-score profile) u10))
          })
        )
      )
      true
    )
    (ok true)
  )
)

;; Apply skill decay
(define-public (apply-skill-decay (user principal) (skill-name (string-ascii 50)))
  (let (
    (skill (unwrap! (map-get? skills { user: user, skill-name: skill-name }) err-not-found))
    (blocks-since-verification (- stacks-block-height (get verification-date skill)))
  )
    (asserts! (> blocks-since-verification u2016) err-invalid-input) ;; ~2 weeks
    
    (let ((decayed-score (/ (* (get score skill) (- u100 skill-decay-rate)) u100)))
      (map-set skills
        { user: user, skill-name: skill-name }
        (merge skill { 
          score: decayed-score,
          decay-factor: (+ (get decay-factor skill) skill-decay-rate)
        })
      )
    )
    (ok true)
  )
)

;; Mark loan as defaulted (only contract owner)
(define-public (mark-loan-defaulted (loan-id uint))
  (let (
    (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
    (borrower (get borrower loan))
    (profile (unwrap! (map-get? user-profiles { user: borrower }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (is-eq (get status loan) "active") err-loan-not-active)
    (asserts! (> stacks-block-height (get due-at loan)) err-invalid-input)
    
    ;; Mark loan as defaulted
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "defaulted" })
    )
    
    ;; Penalize borrower reputation
    (map-set user-profiles
      { user: borrower }
      (merge profile { 
        reputation-score: (if (> (get reputation-score profile) u50)
                              (- (get reputation-score profile) u50)
                              u0),
        is-active: false
      })
    )
    (ok true)
  )
)

;; Emergency pause (only contract owner)
(define-public (set-paused (paused-state bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set paused paused-state)
    (ok true)
  )
)

;; read only functions

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get skill info
(define-read-only (get-skill (user principal) (skill-name (string-ascii 50)))
  (map-get? skills { user: user, skill-name: skill-name })
)

;; Get loan details
(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

;; Get vouch info
(define-read-only (get-vouch (voucher principal) (borrower principal))
  (map-get? vouches { voucher: voucher, borrower: borrower })
)

;; Get current loan ID
(define-read-only (get-next-loan-id)
  (var-get next-loan-id)
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-loans-issued: (var-get total-loans-issued),
    total-repaid: (var-get total-repaid),
    platform-fee: (var-get platform-fee),
    next-loan-id: (var-get next-loan-id)
  }
)

;; Check if user can get loan
(define-read-only (can-get-loan (user principal) (amount uint))
  (match (calculate-credit-score user)
    credit-score (ok (and 
      (<= amount max-loan-amount)
      (>= credit-score min-reputation-score)
    ))
    error (err error)
  )
)

;; private functions

;; Calculate interest rate based on credit score
(define-private (calculate-interest-rate (credit-score uint))
  (if (>= credit-score u800)
    base-interest-rate
    (if (>= credit-score u600)
      (+ base-interest-rate u200)
      (if (>= credit-score u400)
        (+ base-interest-rate u400)
        (+ base-interest-rate u600)
      )
    )
  )
)