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

(define-constant max-loan-amount u1000000) ;; 1M microSTX
(define-constant min-reputation-score u100)
(define-constant skill-decay-rate u5) ;; 5% per period
(define-constant base-interest-rate u500) ;; 5% in basis points

;; data vars
(define-data-var next-loan-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points
(define-data-var total-loans-issued uint u0)
(define-data-var total-repaid uint u0)

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
