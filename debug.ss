#lang scheme

(provide %%v %%)

(require "util.ss")

(define %%-nesting (make-parameter 0))

(define (%%v v)
  (display (indent '%% v))
  v)

(define (%% proc . args)
  (define number-of-spaces (* 3 (%%-nesting)))
  (display (indent/bullet '%% number-of-spaces (pretty-format (cons proc args))))
  (let ([result (parameterize ([%%-nesting (add1 (%%-nesting))]) (apply proc args))])
    (display (indent/bullet '-> number-of-spaces (pretty-format result)))
    result))

