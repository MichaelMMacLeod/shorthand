#lang racket

(provide
 (contract-out [shorten (-> (listof (or/c exact-arity-procedure?
                                          just?
                                          (not/c procedure?)))
                            any/c)])
 (rename-out [shorthand-module-begin #%module-begin]
             [shorthand-app #%app]
             [shorthand-let let]
             [shorthand-lambda lambda]
             )
 (all-from-out "exact-arity-functions.rkt")
 (except-out (all-from-out racket)
             let
             lambda
             #%module-begin
             #%app)
 )

(require "just.rkt" "exact-arity-functions.rkt")

(define-syntax (shorthand-lambda stx)
  (syntax-case stx ()
    [(_ (arg ...) body ...)
     #'(just (lambda (arg ...)
               (shorten body ...)))]))

(define-syntax (shorthand-let stx)
  (syntax-case stx ()
    [(_ ([id val-expr ...] ...) body ...)
     #'(let ([id (shorten val-expr ...)] ...)
         (shorten body ...))]
    [(_ proc-id ([id init-expr ...] ...) body ...)
     #'(let proc-id ([id (shorten init-expr ...)] ...)
         (shorten body ...))]
    ))

(define (shorten-with-extra xs)
  (match xs
    [(list head tail ...)
     (cond
       [(procedure? head)
        (define arity (procedure-arity head))
        (cond
          [(integer? arity)
           (let loop ([n arity]
                      [input tail]
                      [args null])
             (cond
               [(= n 0)
                (values (apply head (reverse args))
                        input)]
               [else
                (define-values (arg rest-of-input)
                  (shorten-with-extra input))
                (loop (- n 1)
                      rest-of-input
                      (cons arg args))]))]
          [else
           (raise-argument-error 'short
                                 "integer?"
                                 arity)])]
       [(just? head)
        (values (just-value head)
                tail)]
       [else
        (values head tail)])]
    [(list) (values (void) (void))]))

(define (shorten . xs)
  (let-values ([(result rest-of-input) (shorten-with-extra xs)])
    result))

(define-syntax-rule (shorthand-module-begin form ...)
  (#%module-begin (shorten form ...)))

(define-syntax-rule (shorthand-app form ...)
  (just form ...))

(module reader syntax/module-reader
  shorthand)