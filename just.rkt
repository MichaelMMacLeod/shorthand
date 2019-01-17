#lang racket/base

(require racket/contract)

(provide
 (contract-out [exact-arity-procedure? contract?]
               (struct just ([value exact-arity-procedure?]))))

(struct just (value))

(define (exact-arity-procedure? f)
  (integer? (procedure-arity f)))