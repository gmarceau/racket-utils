#lang racket
(provide (all-defined-out))


(provide list-even-length/c)
(define list-even-length/c (rename-contract (and/c list? (lambda (lst) (even? (length lst))))
                                            'list-even-length/c))

(define (map-pairwise k-fn v-fn lst)
  (let loop ([even #f] [lst lst])
    (cond [(empty? lst) empty]
          [(not even) (cons (k-fn (first lst)) (loop (not even) (rest lst)))]
          [else (cons (v-fn (first lst)) (loop (not even) (rest lst)))])))

(define (list-pairwise/c k/c v/c)
  (define name (build-compound-type-name 'list-pairwise/c k/c v/c))
  (rename-contract
   (and/c list-even-length/c
          (make-contract #:name name
                         #:first-order (lambda (lst) (andmap identity (map-pairwise (contract-first-order k/c) (contract-first-order v/c) lst)))
                         #:projection (lambda (blame) (lambda (lst) (map-pairwise ((contract-projection k/c) blame) ((contract-projection v/c) blame) lst)))))
   name))