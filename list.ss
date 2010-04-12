#lang scheme
(require gmarceau/cut
         gmarceau/hash)
(provide (all-defined-out))

(define (mapmap fn lstlst)
  (map (// map fn <>) lstlst))

(define (map^3 fn lstlstlst)
  (mapmap (// map fn <>) lstlstlst))

(define (list->dotted-pair lst) (cons (first lst) (second lst)))

(define (lstlst->assoc lstlst) (map list->dotted-pair lstlst))

(define (transpose lstlst) (apply map list lstlst))

(define (take-percentile lst percentile top? key-fn cache-keys?)
  (define sorted (sort lst (if top? > <) #:key key-fn #:cache-keys? cache-keys?))
  (define best (first sorted))
  (define cut-off (* percentile (key-fn best)))
  (define cut-off-index
    (for/first ([item sorted]
                [i (in-naturals)]
                #:when (< (key-fn item) cut-off))
      i))
  (if cut-off-index
      (take sorted cut-off-index)
      sorted))

(define (take-top-percentile lst percentile
                             #:key [key-fn (lambda (x) x)]
                             #:cache-keys? [cache-keys? false])
  (take-percentile lst (- 1 percentile) #t key-fn cache-keys?))

(define (take-bottom-percentile lst percentile
                                #:key [key-fn (lambda (x) x)]
                                #:cache-keys? [cache-keys? false])
  (take-percentile lst percentile #f key-fn cache-keys?))


(define (shuffle lst) (sort lst < #:key (lambda (v) (random)) #:cache-keys? #t))

(define (struct->list str) (vector->list (struct->vector str)))

; (listof a) (a -> b) -> (list/c b (listof a))
(define (group-by lst key-fn)
  (hash->list (group-by:h lst key-fn)))

; (listof a) (a -> b) -> (hash b (listof a))
(define (group-by:h lst key-fn)
  (for/fold ([result (make-immutable-hash empty)])
    ([item lst])
    (hash-update result (key-fn item) (// cons item <>) empty)))

