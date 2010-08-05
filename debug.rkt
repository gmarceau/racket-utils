#lang racket

(provide %%v %% pretty-print*)

(require "util.rkt")

(define %%-nesting (make-parameter 0))

(define (%%v . vs)
  (display (indent/bullet
            '%% (* 3(%%-nesting))
            (string-join (map pretty-format vs) " ")))
  (last vs))

(define (%% proc . args)
  (define number-of-spaces (* 3 (%%-nesting)))
  (display (indent/bullet '%% number-of-spaces (pretty-format (cons proc args))))
  (let ([result (parameterize ([%%-nesting (add1 (%%-nesting))]) (apply proc args))])
    (display (indent/bullet '-> number-of-spaces (pretty-format result)))
    result))

(define (pretty-print* . vs)
  (pretty-print vs))
