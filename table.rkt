#lang racket

(require
 (planet neil/csv)
 (planet dherman/csv-write/csv-write)
 "cut.rkt"
 "hash.rkt"
 "counting.rkt"
 "util.rkt"
 (only-in srfi/1 lset-difference))
(provide (all-defined-out) unique)

; a table is a (listof (hash symbol any))

(define (table/c value/c)
  (listof (hash/c symbol? value/c)))

(require "debug.rkt")
#|
(define (rowof* . key-values/c)
  (define keys (map first key-values/c))
  (define values/c (map second key-values/c))
  
  (define (first-order v)
    (and (%% hash? v)
         (%% lset<= equal? keys (hash-keys v))))
  
  (define (projection pos neg src-info name positive-position?)
    (lambda (v)
      (define extra-keys (lset-difference equal? (hash-keys v) keys))
      (define wrapped
        (for/fold ([result empty-hash]) ([k keys] [value/c values/c])
          (hash-set result k (contract value/c (hash-ref v k) pos neg src-info))))
      (for/fold ([result wrapped]) ([k extra-keys])
        (hash-set result k (hash-ref v k)))))

  (make-proj-contract (build-compound-type-name 'rowof values/c)
                      projection first-order))
|#

(define-syntax rowof
  (syntax-rules ()
    [(_ (key val/c) ...)
     (rowof* (list 'key val/c) ...)]))

(define (tableof* row/c) (listof row/c))

(define-syntax tableof
  (syntax-rules ()
    [(_ (key val/c) ...)
     (listof (rowof (key val/c) ...))]))

(define !! hash-set)
(define -- hash-remove)
(define ?? hash-has-key?)
(define none-given (gensym))
(define (.. hash #:default [default none-given] . fields)
  (for/fold ([result hash]) ([f fields])
    (cond [(eq? default none-given) (hash-ref result f)]
          [(procedure? default) (hash-ref result f default)]
          [else (hash-ref result f (lambda () default))])))

(define (csv->table input)
  (define lstlst (csv->list input))
  (lstlst->table (map string->symbol (first lstlst)) (rest lstlst)))

(define (table->csv table #:port [port (current-output-port)] #:order [order-in #f])
  (define header (hash-keys (first table)))
  (define order (or order-in header))
  (define missing (lset-difference equal? header order))
  (define extra (lset-difference equal? order header))
  (define dups (filter-map (lambda (v) (and (> (counted-c v) 1)
                                            (counted-cats v)))
                           (count-instances order)))
  
  (define order/missing (append order missing))
  (when (not (empty? extra)) (error 'table->csv "order specification given superfluously for: ~s" extra))
  (when (not (empty? dups)) (error 'table->csv "order specification given in duplicate for: ~s" dups))
  (let ()
    (define data (for/list ([row table])
                   (map (// .. row <>) order/missing)))
    (write-table (cons order/missing data) port)))

(define (lstlst->table field-names lstlst)
  (for/list ([lst lstlst])
    (make-immutable-hash
     (map cons field-names lst))))

;; WHEN-SPEC is (listof (list/c field-name (or/c value (any . -> . boolean?)))
(define (select table #:field [field #f] #:fields [fields #f]
                #:sort [sort-field #f]
                #:sort-fn [sort-fn <]
                #:sort-key [sort-key (lambda (x) x)]
                #:sort-cache-keys? [sort-cache-keys? #f]
                . when-specs)
  (define (matches? i)
    (andmap
     (match-lambda [(list f (? procedure? fn)) (fn (hash-ref i f))]
                   [(list f v) (equal? (hash-ref i f) v)])
     (group-pairwise when-specs)))
  
  (define targetted (filter matches? table))
  (define sorted (if sort-field
                     (sort targetted sort-fn #:key (lambda (v) (sort-key (.. v sort-field))) #:cache-keys? sort-cache-keys?)
                     targetted))
  
  (when (and field fields)
    (error 'select "only one of #:field or #:fields is allowed"))
  
  (for/list ([i sorted])
    (cond [field (hash-ref i field)]
          [fields (make-immutable-hash
                   (for/list ([f fields]) (cons f (hash-ref i f))))]
          [else i])))

;; find: Like select, but checks that only one result is found.
(define (find table #:fields [fields #f] . when-specs)
  (match (apply select table #:fields fields when-specs)
    [(list) (error 'find "nothing found: ~a" when-specs)]
    [(list v) v]
    [many (error 'find "more than one found: ~a" many)]))

(define (table-index-by table field)
  (make-immutable-hash
   (for/list ([i table])
     (cons (hash-ref i field) i))))

(define (table-sort-by table field less-than)
  (sort table less-than #:key (// hash-ref <> field)))

(define (table-group-by table field)
  (for/fold ([result empty-hash]) ([i table])
    (hash-update result (hash-ref i field) (// cons i <>) empty)))

(define (join-on left-table right-table left-field
                 #:right-field [right-field left-field]
                 #:missing [missing 'error]
                 #:duplicate [duplicate 'error])
  (define indexed (table-index-by right-table right-field))
  (filter (lambda (i) i)
          (for/list ([i left-table])
            (let ([left-val (hash-ref i left-field)])
              (if (hash-has-key? indexed left-val)
                  (for/fold ([result i]) ([(key val) (hash-ref indexed left-val)])
                    (cond [(or (not (hash-has-key? result key))
                               (eq? duplicate 'right))
                           (hash-set result key val)]
                          [(eq? duplicate 'left) result]
                          [(eq? duplicate 'error)
                           (error 'join-on "joined tables duplicates field ~a" key)]
                          [else (error 'join-on "duplicate is not one of 'left, 'right, or 'error")]))
                    
                  (match missing
                    ['partial i]
                    ['remove #f]
                    ['error (error 'join-on "join target doesn't exists in the right table: ~a" left-val)]))))))

(define (join/nested left-table right-table left-field
                     #:right-field [right-field left-field]
                     #:missing [missing 'error])
  (define indexed (table-index-by right-table right-field))
  (filter (lambda (i) i)
          (for/list ([i left-table])
            (let ([left-val (hash-ref i left-field)])
              (if (hash-has-key? indexed left-val)
                  (hash-set i left-field (hash-ref indexed left-val))
                  (match missing
                    ['partial i]
                    ['remove #f]
                    ['error (error 'join-on "join target doesn't exists in the right table: ~a" left-val)]))))))

(define (join/many left-table right-table left-field #:right-field [right-field left-field])
  (define grouped (table-group-by right-table right-field))
  (for/list ([i left-table])
    (let ([lst (hash-ref grouped (hash-ref i left-field))])
      (hash-set i left-field lst))))

(define (table-add-column table field v/fn)
  (for/list ([i table])
    (if (procedure? v/fn)
        (hash-set i field (v/fn i))
        (hash-set i field v/fn))))

(define (table-remove-column table . fields)
  (for/list ([i table]) (for/fold ([result i]) ([f fields]) (hash-remove result f))))

(define (table-fill-missing table field v/fn)
  (for/list ([i table])
    (cond [(hash-has-key? i field) i]
          [(procedure? v/fn) (hash-set i field (v/fn i))]
          [else (hash-set i field v/fn)])))

(define (table-update table field v/fn)
  (map (// hash-update <> field v/fn) table))

(define (table-rename-column table old-name new-name)
  (for/list ([i table])
    (hash-set (hash-remove i old-name)
              new-name (hash-ref i old-name))))

(define (column->list table field)
  (map (lambda (i) (.. i field)) table))

(define unique (procedure-rename remove-duplicates 'unique))
