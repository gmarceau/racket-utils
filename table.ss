#lang scheme

(require
 (planet neil/csv)
 (planet dherman/csv-write/csv-write)
 "cut.ss"
 "hash.ss"
 "utils.ss"
 srfi/1
 )
(provide (all-defined-out))

; a table is a (listof (hash symbol any))

(define (table/c value/c)
  (listof (hash/c symbol? value/c)))

#|
(define (rowof key-values/c)
  (define keys (map first key-values/c)
  (define values/c (map second key-values/c))
  (flat-named-contract
   'row
   (lambda (v)
     (and (hash? v)
          (lset<= (hash-keys v)
                  keys/c)
          (andmap
           (lambda (k value/c)
             (value/c (hash-ref v k)))
           keys values/c))))))
     

(define-syntax (tableof stx)
  (syntax-case stx ()
    [(_ [key value/c] ...)
     #'(flat-named-contract
        (and (listof
              (
  |#


(define .. hash-ref)
(define !! hash-set)
(define ?? hash-has-key?)
   

(define (csv->table input)
  (define lstlst (csv->list input))
  (lstlst->table (map string->symbol (first lstlst)) (rest lstlst)))

(define (table->csv table #:port [port (current-output-port)] #:order [order #f])
  (define header (hash-keys (first table)))
  (when (and order
             (not (lset= header order)))
    (error 'table->csv "ordering fields do not match the table: ~a vs ~a" order header))
  (let ()
    (define data (for/list ([row table])
                   (map (// .. row <>) (or order header))))
    (write-table (cons (or order header) data) port)))

(define (lstlst->table field-names lstlst)
  (for/list ([lst lstlst])
    (make-immutable-hash
     (map cons field-names lst))))

;; WHEN-SPEC is (listof (list/c field-name value))
(define (select table #:when [when-spec empty] . fields)
  (define (matches? i)
    (andmap (match-lambda [(list f v) (equal? (hash-ref i f) v)]) when-spec))
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