#lang racket

(provide for/append)
(define-syntax (for/append stx)
  (syntax-case stx ()
    [(_ (seq ...) expr ...)
     (syntax/loc stx
       (apply append (for/list (seq ...) expr ...)))]))
