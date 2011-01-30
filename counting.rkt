#lang racket
(require "hash.rkt"
         "util.rkt"
         "cut.rkt"
         unstable/function)

(provide (struct-out counted))
(define-struct counted (cats c) #:prefab)

; (listof a) -> (listof (struct count))
(provide/contract [count-instances (list? . -> . (listof counted?))])
(define (count-instances lst)
  (sort
   (hash-map
    (count-instances:h lst)
    make-counted)
   > #:key counted-c))

(provide/contract [count-instances:h (list? . -> . (hash/c any/c natural-number/c))])
(define (count-instances:h lst)
  (for/fold ([result (make-immutable-hash empty)])
    ([item lst])
    (hash-update result item add1 0)))

(provide/contract [counts->list ((listof counted?) . -> . (listof (list/c any/c natural-number/c)))])
(define (counts->list counts)
  (map (match-lambda [(struct counted (cats c)) (list cats c)])
       counts))

(provide/contract [normalize-counts ((listof counted?) . -> . (listof counted?))])
(define (normalize-counts counts)
  (define total (apply + (map (// counted-c <>) counts)))
  (map (match-lambda [(struct counted (cats c)) (make-counted cats (/ c total))])
       counts))

(provide time-serie-histogram)
(define (time-serie-histogram histograms #:sort? [sort? #t])
  (define (invert-lists rows)
    (apply map (lambda m m) rows))
  
  (define (complete-histogram cats h)
    (define missing-cats (remove* (map counted-cats h) cats))
    (append h (map (// make-counted <> 0) missing-cats)))
  
  (define (histogram->hash h)
    (make-immutable-hash (map (lambda (i) (cons (counted-cats i) (counted-c i))) h)))
  
  (define all-cats (remove-duplicates (map counted-cats (flatten histograms))))
  (define completed-histograms (map (// complete-histogram all-cats <>) histograms))
  (define as-hash (map histogram->hash completed-histograms))
  
  (define time-series
    (map (lambda (cat) (cons cat (map (// hash-ref <> cat) as-hash)))
         all-cats))
  
  (define sorted 
    (if (not sort?)
        time-series
        (sort
         time-series
         > #:key (lambda (lst) (apply + (rest lst))) #:cache-keys? #t)))
  
  sorted)

(define-struct bucket (low high c) #:prefab)
(provide value-histogram)
(define (value-histogram lst number-of-buckets
                         #:min-v [given-min-v #f]
                         #:max-v [given-max-v #f]
                         #:key [fn (lambda (i) i)])
  (define keyed (map fn lst))
  (define min-v (or given-min-v (apply min keyed)))
  (define max-v (or given-max-v (apply max keyed)))
  (define delta-per-bucket (/ (- max-v min-v) number-of-buckets))
  (define (bucket-of v)
    (cond [(< v min-v) 'min]
          [(< max-v v) 'max]
          [else (truncate (/ (- v min-v) delta-per-bucket))]))
  (define (category-of bucket-id)
    (match bucket-id
      ['min (format "less than ~a" (exact->inexact min-v))]
      ['max (format "more than ~a" (exact->inexact max-v))]
      [else (format "~a to ~a" 
                    (exact->inexact (+ min-v (* bucket-id delta-per-bucket)))
                    (exact->inexact (+ min-v (* (add1 bucket-id) delta-per-bucket))))]))
  (define h (for/fold ([result empty-hash])
              ([v keyed])
              (hash-update result (bucket-of v) add1 0)))
  (for/list ([i `(min ,@(build-list number-of-buckets identity) max)])
    (make-counted (category-of i) (hash-ref h i 0))))



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

(provide/contract [take-top-percentile ((list? number?)
                                        (#:key (any/c . -> . any/c) #:cache-keys? boolean?) . ->* . list?)])
(define (take-top-percentile lst percentile
                             #:key [key-fn identity]
                             #:cache-keys? [cache-keys? false])
  (take-percentile lst (- 1 percentile) #t key-fn cache-keys?))

(provide/contract [take-bottom-percentile ((list? number?)
                                           (#:key (any/c . -> . any/c) #:cache-keys? boolean?) . ->* . list?)])
(define (take-bottom-percentile lst percentile
                                #:key [key-fn identity]
                                #:cache-keys? [cache-keys? false])
  (take-percentile lst percentile #f key-fn cache-keys?))


(define (median lst) (list-ref lst (round (/ (length lst) 2))))

(define (mode lst)
  (counted-cats (first (count-instances lst))))