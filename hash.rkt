#lang racket
(require "util.rkt")
(provide (all-defined-out))

(define empty-hash (make-immutable-hash empty))

(define (hash-ref-all h pairs)
  (map (lambda (k) (list k (hash-ref h k))) pairs))

(define (hash-set-all h pairs)
  (for/fold ([h h]) ([p pairs])
    (match p [(list k v) (hash-set h k v)])))

(define (hash-map:h hash fn) (list->hash (hash-map hash fn)))

(define (hash-map-values:h hash proc)
  (for/fold ([result empty-hash]) ([(k v) hash])
    (hash-set result k (proc v))))

(define (hash-map-values hash proc)
  (hash->list (hash-map-values:h hash proc)))

(define (hash-map-keys hash proc)
  (hash->list (hash-map-keys:h hash proc)))

(define (hash-map-keys:h hash proc)
  (for/fold ([result empty-hash]) ([(k v) hash])
    (let ([new-key (proc k)])
      (when (hash-has-key? result new-key)
        (error 'hash-map-keys:h "duplicated destination key: ~a" new-key))
      (hash-set result new-key v))))

(define (hash->list hash #:sort [sort-fn #f] #:key [key-fn id] #:cache-keys? [cache-keys? #f])
  (define result (hash-map hash list))
  (if sort-fn (sort result sort-fn #:key (compose key-fn first) #:cache-keys? cache-keys?) result))

(define (hash->assoc hash) (hash-map hash cons))
(define (list->hash lst) (make-immutable-hash (map (match-lambda [(list a b) (cons a b)]) lst)))
(define assoc->hash make-immutable-hash)
(define (hash-keys hash) (hash-map hash (lambda (k v) k)))
(define (hash-values hash) (hash-map hash (lambda (k v) v)))

(define (list->hash-set lst) (for/fold ([result empty-hash]) ([i lst]) (hash-set result i #t)))