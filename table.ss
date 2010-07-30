#lang scheme

(require
 (planet neil/csv)
 (planet dherman/csv-write/csv-write)
 (planet untyped/unlib/for)
 "cut.ss"
 "hash.ss"
 "util.ss"
 "counting.ss"
 srfi/1
 )
(provide (all-defined-out))

; a table is a (listof (hash symbol any))

(define (table/c value/c)
  (listof (hash/c symbol? value/c)))

(require "debug.ss")
#;
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

(define-syntax rowof
  (syntax-rules ()
    [(_ (key val/c) ...)
     (rowof* (list 'key val/c) ...)]))

(define (tableof* row/c) (listof row/c))

(define-syntax tableof
  (syntax-rules ()
    [(_ (key val/c) ...)
     (listof (rowof (key val/c) ...))]))

(define .. hash-ref)
(define !! hash-set)
(define -- hash-remove)
(define ?? hash-has-key?)


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
  (when (not (empty? missing)) (error 'table->csv "order specification missing for: ~s" missing))
  (when (not (empty? extra)) (error 'table->csv "order specification given superfluously for: ~s" extra))
  (when (not (empty? dups)) (error 'table->csv "order specification given in duplicate for: ~s" dups))
  (let ()
    (define data (for/list ([row table])
                   (map (// .. row <>) (or order header))))
    (write-table (cons (or order header) data) port)))

(define (lstlst->table field-names lstlst)
  (for/list ([lst lstlst])
    (make-immutable-hash
     (map cons field-names lst))))

;; WHEN-SPEC is (listof (list/c field-name (or/c value (any . -> . boolean?)))
(define (select table #:when [when-spec empty] . fields)
  (define (matches? i)
    (andmap
     (match-lambda [(list f (? procedure? fn)) (fn (hash-ref i f))]
                   [(list f v) (equal? (hash-ref i f) v)])
     when-spec))
  (for/list ([i table] #:when (matches? i))
    (if (empty? fields)
        i
        (make-immutable-hash
         (for/list ([f fields]) (cons f (hash-ref i f)))))))

(define (index-on table field)
  (make-immutable-hash
   (for/list ([i table])
     (cons (hash-ref i field) i))))

(define (sort-on table field less-than)
  (sort table less-than #:key (// hash-ref <> field)))

(define (group-on table field)
  (for/fold ([result empty-hash]) ([i table])
    (hash-update result (hash-ref i field) (// cons i <>) empty)))

(define (join-on left-table right-table left-field
                 #:right-field [right-field left-field]
                 #:missing? [missing? 'partial])
  (define indexed (index-on right-table right-field))
  (filter (lambda (i) i)
          (for/list ([i left-table])
            (let ([left-val (hash-ref i left-field)])
              (if (hash-has-key? indexed left-val)
                  (for/fold ([result i]) ([(key val) (hash-ref indexed left-val)])
                    (hash-set result key val))
                  (match missing?
                    ['partial i]
                    ['remove #f]
                    ['error (raise "join target doesn't exists in the right table")]))))))

(define (join/nested left-table right-table left-field
                     #:right-field [right-field left-field]
                     #:missing? [missing? 'partial])
  (define indexed (index-on right-table right-field))
  (filter (lambda (i) i)
          (for/list ([i left-table])
            (let ([left-val (hash-ref i left-field)])
              (if (hash-has-key? indexed left-val)
                  (hash-set i left-field (hash-ref indexed left-val))
                  (match missing?
                    ['partial i]
                    ['remove #f]
                    ['error (raise "join target doesn't exists in the right table")]))))))

(define (join/many left-table right-table left-field #:right-field [right-field left-field])
  (define grouped (group-on right-table right-field))
  (for/list ([i left-table])
    (let ([lst (hash-ref grouped (hash-ref i left-field))])
      (hash-set i left-field lst))))

(define (table-add-column table field data)
  (for/list ([i table] [d data])
    (hash-set i field d)))

(define (table-remove-column table . fields)
  (for/list ([i table]) (for/fold ([result i]) ([f fields]) (hash-remove result f))))

(define (table-fill-missing table field v)
  (for/list ([i table])
    (hash-update i field id v)))

(define (table-update table field fn)
  (map (// hash-update <> field fn) table))

(define (table-rename-column table old-name new-name)
  (for/list ([i table])
    (hash-set (hash-remove i old-name)
              new-name (hash-ref i old-name))))