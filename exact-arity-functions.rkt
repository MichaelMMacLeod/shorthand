#lang racket

(require (for-syntax racket/base racket/syntax)
         racket/provide-syntax)

(define-syntax (fix-arities stx)
  (syntax-case stx ()
    [(_ (functions arities) ...)
     (with-syntax
         ([(define-names ...)
           (for/list ([function (in-list (syntax->list #'(functions ...)))])
             (format-id function #:source function
                        "fixed-arity-~a" (syntax-e function)))])
       #'(begin
           (provide (rename-out [define-names functions] ...))
           (define-values (define-names ...)
             (values (procedure-reduce-arity functions arities) ...))))]))

(fix-arities
 (+ 2)
 (- 2)
 (* 2)
 (/ 2)
 (range 2)
 (map 2)
 (number->string 1)
 )