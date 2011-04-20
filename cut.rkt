#lang racket

(require (rename-in srfi/26 [cut //]))
(provide (all-from-out srfi/26)
         pipe lambda-pipe)

(define-syntax (pipe stx)
  (syntax-case stx ()
    [(_ init (callee args ...) ...)
     (syntax/loc stx
       (for/fold ([result init]) ([fn (list (// callee args ...) ...)])
         (fn result)))]))

(define-syntax (lambda-pipe stx)
  (syntax-case stx ()
    [(_ (callee args ...) ...)
     (syntax/loc stx
       (lambda (init) (pipe init (callee args ...) ...)))]))

