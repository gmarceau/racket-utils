#lang racket
(require "cut.rkt"
         "hash.rkt"
         "util.rkt")

(provide/contract
 [mapmap ((any/c . -> . any) (listof list?) . -> . (listof list?))])
(define (mapmap fn lstlst)
  (map (// map fn <>) lstlst))

(provide/contract
 [map^3 ((any/c . -> . any) (listof (listof list?)) . -> . (listof (listof list?)))])
(define (map^3 fn lstlstlst)
  (mapmap (// map fn <>) lstlstlst))

(provide/contract
 [map^n ((any/c . -> . any) list? integer? . -> . list?)])
(define (map^n fn nested-list level)
  (if (= level 1)
      (map fn nested-list)
      (map (// map^n fn <> (sub1 level)) nested-list)))

(provide/contract
 [append^n (list? integer? . -> . list?)])
(define (append^n nested-list level)
  (if (= level 0)
      nested-list
      (append^n (append* nested-list) (sub1 level))))

(provide/contract
 [list->dotted-pair ((list/c any/c any/c) . -> . cons?)])
(define (list->dotted-pair lst) (cons (first lst) (second lst)))

(provide/contract
 [lstlst->assoc ((listof list?) . -> . (listof cons?))])
(define (lstlst->assoc lstlst) (map list->dotted-pair lstlst))

(provide/contract
 [transpose ((listof list?) . -> . (listof list?))])
(define (transpose lstlst) (apply map list lstlst))

(provide/contract
 [shuffle (list? . -> . list?)])
(define (shuffle lst) (sort lst < #:key (lambda (v) (random)) #:cache-keys? #t))

(provide struct->list)
(define (struct->list str) (vector->list (struct->vector str)))

; (listof a) (a -> b) -> (list/c b (listof a))
(provide group-by)
(define (group-by lst key-fn #:map [map-fn id])
  (hash->list (group-by:h lst key-fn #:map map-fn)))

; (listof a) (a -> b) -> (hash b (listof a))
(provide group-by:h)
(define (group-by:h lst key-fn #:map [map-fn id])
  (for/fold ([result (make-immutable-hash empty)])
    ([item lst])
    (hash-update result (key-fn item) (// cons (map-fn item) <>) empty)))
