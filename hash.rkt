#lang racket
(require "list.rkt"
         unstable/function)

(provide empty-hash)
(define empty-hash (make-immutable-hash empty))


#|
(provide/contract [hash (() () #:rest list-even-length?/c . ->* . hash?)])
(define (hash . k/vs)
  (for/fold ([result empty-hash]) ([p (group-pairwise k/vs)])
    (match p [(list k v) (hash-set result k v)])))
|#
(provide/contract [hash-set-all (hash? listof-kv/c . -> . hash?)])
(define (hash-set-all h pairs)
  (for/fold ([h h]) ([p pairs])
    (match p [(list k v) (hash-set h k v)])))

(provide/contract [hash-map:h (hash? (any/c any/c . -> . any/c) . -> . hash?)])
(define (hash-map:h hash fn) (list->hash (hash-map hash fn)))

(provide/contract [hash-map-values:h (hash? (any/c . -> . any/c) . -> . hash?)])
(define (hash-map-values:h hash proc)
  (for/fold ([result empty-hash]) ([(k v) hash])
    (hash-set result k (proc v))))

(provide/contract [hash-map-values (hash? (any/c . -> . any/c) . -> . listof-kv/c)])
(define (hash-map-values hash proc)
  (hash->list (hash-map-values:h hash proc)))

(provide/contract [hash-map-keys (hash? (any/c . -> . any/c) . -> . listof-kv/c)])
(define (hash-map-keys hash proc)
  (hash->list (hash-map-keys:h hash proc)))

(provide/contract [hash-map-keys:h (hash? (any/c . -> . any/c) . -> . hash?)])
(define (hash-map-keys:h hash proc)
  (for/fold ([result empty-hash]) ([(k v) hash])
    (let ([new-key (proc k)])
      (when (hash-has-key? result new-key)
        (error 'hash-map-keys:h "duplicated destination key: ~a" new-key))
      (hash-set result new-key v))))

(provide/contract [hash->list ((hash?) (#:sort (any/c any/c . -> . boolean?)
                                               #:key (any/c . -> . any/c)
                                               #:cache-keys? boolean?)
                                       . ->* . listof-kv/c)])
(define (hash->list hash #:sort [sort-fn #f] #:key [key-fn identity] #:cache-keys? [cache-keys? #f])
  (define result (hash-map hash list))
  (if sort-fn (sort result sort-fn #:key (compose key-fn first) #:cache-keys? cache-keys?) result))

(provide/contract [hash->assoc (hash? . -> . (listof cons?))])
(define (hash->assoc hash) (hash-map hash cons))

(provide/contract [list->hash (listof-kv/c . -> . hash?)])
(define (list->hash lst) (make-immutable-hash (map (match-lambda [(list a b) (cons a b)]) lst)))

(provide/contract [assoc->hash ((listof cons?) . -> . hash?)])
(define assoc->hash make-immutable-hash)

(provide/contract [hash-keys (hash? . -> . list?)])
(define (hash-keys hash) (hash-map hash (lambda (k v) k)))

(provide/contract [hash-values (hash? . -> . list?)])
(define (hash-values hash) (hash-map hash (lambda (k v) v)))

(provide/contract [hash-size (hash? . -> . natural-number/c)])
(define (hash-size h) (length (hash-keys h)))


(require (for-syntax
          racket/match
          racket/list
          racket/pretty
          "list.rkt"))

(provide (rename-out [hash* hash]))
(define-match-expander hash*
  (lambda (stx)
    (syntax-case stx ()
      [(_ arg ... k v ooo)
       (and (identifier? #'ooo)
            (free-identifier=? #'(... ...) #'ooo))
       (with-syntax ([(grouped ...) (group-pairwise (syntax->list #'(arg ...)))])
         (syntax/loc stx (hash-table grouped ... (k v) ooo)))]
      [(_ arg ...)
       (with-syntax ([(grouped ...) (group-pairwise (syntax->list #'(arg ...)))])
         (syntax/loc stx (hash-table grouped ...)))]))
  (lambda (stx) (syntax-case stx () [(_ v ...) #'(hash v ...)] [_ #'hash])))




