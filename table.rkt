#lang racket

(require
 (planet neil/csv)
 (planet dherman/csv-write/csv-write)
 "test.rkt"
 "cut.rkt"
 "hash.rkt"
 "counting.rkt"
 "util.rkt"
 "list.rkt"
 unstable/contract
 unstable/debug
 unstable/regexp
 unstable/function
 unstable/list
 (only-in srfi/1 lset-difference zip unzip2))

(provide unique)

; a table is a (listof (hash symbol any))

(require "debug.rkt")

(define (item/c . keys) (rename-contract (and/c hash? (lambda (v) (andmap (// hash-has-key? v <>) keys))) (cons 'item/c keys)))

(provide/contract [item-kv/c (() () #:rest (list-pairwise/c any/c contract?) . ->* . contract?)])
(define (item-kv/c . key-values)
  (define-values (keys values/c) (unzip2 (group-pairwise key-values)))
  (define name (cons 'item-kv/c (append* (map build-compound-type-name keys values/c))))
  (define (first-order v)
    (and (hash? v)
         (andmap (lambda (k v/c)
                   (let/ec return
                     (contract-first-order-passes? v/c (hash-ref v k (lambda () (return #f))))))
                 keys values/c)))
  
  (define ((projection blame) v)
    (unless (first-order v)
      (raise-blame-error blame v "expected <~a>, given: ~e" name v))
    
    (define extra-keys (lset-difference equal? (hash-keys v) keys))
    (define wrapped
      (for/fold ([result empty-hash]) ([k keys] [v/c values/c])
        (hash-set result k (((contract-projection v/c) blame) (hash-ref v k)))))
    (for/fold ([result wrapped]) ([k extra-keys])
      (hash-set result k (hash-ref v k))))
  
  (make-contract #:name name
                 #:first-order first-order #:projection projection))

(current-test-on? #t)

(test-data
 (define test-c (item-kv/c 1 number? 2 (symbols 'a 'b) 3 (any/c . -> . any/c)))
 (define test-c2 (item-kv/c 1 (number? number? . -> . number?))))

(test 'item/c*
      (check-true (contract-first-order-passes? (item/c 1 2 3) (hash 1 1 2 2 3 3 4 4)))
      (check-false (contract-first-order-passes? (item/c 1 2 3) (hash 2 2 3 3 4 4)))
      (check-contract-exn "(item/c 1)" (item/c 1) (hash 2 2) #f)
      (check-contract-pass (item-kv/c 1 number?) (hash 1 1) #f)
      (check-true (contract-first-order-passes? test-c (hash 1 1 2 'a 3 procedure?)))
      (check-false (contract-first-order-passes? test-c (hash 1 'x 2 'a 3 procedure?)))
      (check-false (contract-first-order-passes? test-c (hash 1 1 2 2 3 procedure?)))
      (check-false (contract-first-order-passes? test-c (hash 1 1 2 'x 3 +)))
      (check-true (contract-first-order-passes? (item/c '(1 2 3)) (hash '(1 2 3) 'x)))
      (check-false (contract-first-order-passes? (item/c '(1 2 3)) (hash '(1 2 3 4) 'x)))
      (check-equal? ((hash-ref (contract test-c2 (hash 1 +) 'pos 'neg) 1) 3 4) 7)
      (check-contract-exn "number? number? number?" test-c2 (hash 1 +) (lambda (v) ((hash-ref v 1) 2 'x)))
      (check-equal? ((hash-ref (contract test-c2 (hash 1 + 2 add1) 'pos 'neg) 2) 3) 4))


(define (table/c . keys) (rename-contract (listof (apply item/c keys)) (cons 'table/c keys)))

(provide/contract [table-kv/c (() () #:rest (list-pairwise/c any/c contract?) . ->* . contract?)])
(define (table-kv/c . key-values/c)
  (define-values (keys values/c) (unzip2 (group-pairwise key-values/c)))
  (rename-contract (listof (apply item-kv/c key-values/c))
                   (cons 'table-kv/c (append* (map build-compound-type-name keys values/c)))))


(define (table? v) (and (list? v) (andmap hash? v)))
(define item? hash?)

(test-data (define test-c3 (table-kv/c 1 (number? number? . -> . number?))))

(test 'table/c
      (check-false (contract-first-order-passes? (table/c 'time) (list (hash 'x 1))))
      (check-true (contract-first-order-passes? (table/c 'time) (list (hash 'time 1))))
      (check-true (contract-first-order-passes? test-c3 (list (hash 1 +))))
      (check-contract-exn (regexp (regexp-quote (format "~a" '(item-kv/c 1 (-> number? number? number?)))))
                          test-c3 (list (hash 1 add1)) #f)
      (check-contract-exn "number? number? number?" test-c3 (list (hash 1 +)) (lambda (v) ((hash-ref (first v) 1) 1 'x))))

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
(define when-spec-contract (listof (list/c any/c (or/c (negate procedure?) predicate-like/c))))
(define select-contract (([table table?])
                         (#:field [field any/c] #:fields [fields list?] #:sort [sort-field any/c]
                                  #:sort-fn [sort-fn comparison-like/c] #:sort-key [sort-key (any/c . -> . any)]
                                  #:sort-cache-keys? [sort-cache-keys? boolean?])
                         #:rest rst when-spec-contract
                         . ->d . [result list?]))
(provide/contract [select select-contract])
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
  
  (define needed-fields (append (or fields (list field))
                                (if sort-field (list sort-field) empty)))
  (for* ([i table]
         [f needed-fields])
    (unless (hash-has-key? i f)
      (error 'select (format "field `~a' is missing in: ~a" f i))))
  
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

(test-data
 (define data (list (hash 'x 1 'y 2 'z 3 'boo "foo") (hash 'x 4 'y 5 'z 6))))

(test 'select
      (define select-wc (contract select-contract select 'pos 'neg))
      (check-equal? (select-wc data #:field 'x) '(1 4))
      (check-equal? (select-wc data #:fields '(x y)) (list (hash 'x 1 'y 2) (hash 'x 4 'y 5)))
      (check-equal? (select-wc data #:field 'x #:sort 'x #:sort-fn >) '(4 1))
      (check-exn-msg exn:fail? "field `aa' is missing" (lambda () (select-wc data #:field 'aa)))
      (check-exn-msg exn:fail? "field `bb' is missing" (lambda () (select-wc data #:fields '(x bb))))
      (check-exn-msg exn:fail? "field `cc' is missing" (lambda () (select-wc data #:field 'x #:sort 'cc))))

;; find: Like select, but checks that only one result is found, then return that item
(provide/contract [find (([table table?]) (#:field [field any/c] #:fields [fields list?]) #:rest rst when-spec-contract . ->d . [result item?])])
(define (find table #:field [field #f] #:fields [fields #f] . when-specs)
  (match (apply select table #:field field #:fields fields when-specs)
    [(list) (error 'find "nothing found: ~a" when-specs)]
    [(list v) v]
    [many (error 'find "more than one found: ~a" many)]))

(provide/contract [table-index-by (([table (table/c field)] [field any/c]) () . ->d . [result (hash/c any/c item?)])])
(define (table-index-by table field)
  (define result (for/list ([i table]) (cons (hash-ref i field) i)))
  (define dup (check-duplicate result #:key car))
  (when dup (error 'table-index-by "duplicate index key: ~a" dup))
  (make-immutable-hash result))

(test 'table-index-by
      (check-equal? (table-index-by (list (hash 'a 1) (hash 'a 2)) 'a)
                    (hash 1 (hash 'a 1) 2 (hash 'a 2)))
      (check-exn-msg exn:fail? "duplicate" (lambda () (table-index-by (list (hash 'a 1) (hash 'a 1)) 'a))))

(provide/contract [table-sort-by (table? any/c comparison-like/c . -> . table?)])
(define (table-sort-by table field less-than #:key [key-fn identity] #:cache-keys? [cache-keys? #f])
  (sort table less-than #:key (lambda (i) (key-fn (hash-ref i field))) #:cache-keys? cache-keys?))

(define table-group-by-contract (([table (table/c field)] [field any/c]) () . ->d . [result (hash/c any/c (table/c field))]))
(provide/contract [table-group-by table-group-by-contract])
(define (table-group-by table field)
  (for/fold ([result empty-hash]) ([i table])
    (hash-update result (hash-ref i field) (// cons i <>) empty)))

(test 'table-group-by
      (check-equal? (table-group-by data 'x) (hash 1 (list (hash 'x 1 'y 2 'z 3 'boo "foo"))
                                                   4 (list (hash 'x 4 'y 5 'z 6))))
      (check-contract-exn "(table/c x)" table-group-by-contract table-group-by (lambda (fn) (fn (list (hash)) 'x))))

(define (or-supply v default) (if (eq? v the-unsupplied-arg) default v))

(define join-on-contract (([left-table (table/c left-field)]
                           [right-table (table/c (or-supply right-field left-field))]
                           [left-field any/c])
                          (#:right-field [right-field any/c]
                                         #:missing [missing (symbols 'error 'partial 'remove)]
                                         #:duplicate [duplicate (symbols 'right 'left 'error)])
                          . ->d . [result (table/c left-field (or-supply right-field left-field))]))
(provide/contract [join-on join-on-contract])
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

(test-data
 (define test-join-left (list (hash 'a 1 'b 2) (hash 'a 4 'b 5)))
 (define test-join-right (list (hash 'c 5 'd 15) (hash 'c 2 'd 10))))

(test 'join-on
      (check-equal? (join-on test-join-left test-join-right 'b #:right-field 'c)
                    (list (hash 'a 1 'b 2 'c 2 'd 10)
                          (hash 'a 4 'b 5 'c 5 'd 15)))
      
      (check-contract-exn "table/c b" join-on-contract join-on
                          (// <> (list (hash 'a 1)) empty 'b))
      (check-contract-exn "table/c b" join-on-contract join-on
                          (// <> empty (list (hash 'a 1)) 'b))
      (check-contract-exn "table/c b" join-on-contract join-on
                          (// <> (list (hash 'a 1)) (list (hash 'a 1)) 'a #:right-field 'b)))

(define join-nested-contact (([left-table (table/c left-field)]
                              [right-table (table/c (or-supply right-field left-field))]
                              [left-field any/c])
                             (#:right-field [right-field any/c]
                                            #:missing [missing (symbols 'partial 'remove 'error)])
                             . ->d . [result (table-kv/c left-field (item/c (or-supply right-field left-field)))]))
(provide/contract [join-nested join-nested-contact])
(define (join-nested left-table right-table left-field
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

(test 'join-nested
      (check-equal? (join-nested test-join-left test-join-right 'b #:right-field 'c)
                    (list (hash 'a 1 'b (hash 'c 2 'd 10))
                          (hash 'a 4 'b (hash 'c 5 'd 15)))))

(provide/contract [join-many (([left-table (table/c left-field)]
                               [right-table (table/c (or-supply right-field left-field))]
                               [left-field any/c])
                              (#:right-field [right-field any/c])
                              . ->d . [result (table-kv/c left-field (listof (item/c (or-supply right-field left-field))))])])
(define (join-many left-table right-table left-field #:right-field [right-field left-field])
  (define grouped (table-group-by right-table right-field))
  (for/list ([i left-table])
    (let ([lst (hash-ref grouped (hash-ref i left-field))])
      (hash-set i left-field lst))))

(test 'join-many
      (check-match (join-many test-join-left (cons (hash 'c 5 'd 20) test-join-right) 'b #:right-field 'c)
                   (list (hash 'a 1 'b (list (hash 'c 2 'd 10)))
                         (hash 'a 4 'b (list-no-order (hash 'c 5 'd 20) (hash 'c 5 'd 15))))))

(provide/contract [table-add-column (([table table?] [field any/c] [v/fn (or/c (negate procedure?) (item? . -> . any))])
                                     () . ->d . [result (table/c field)])])
(define (table-add-column table field v/fn)
  (for/list ([i table])
    (if (procedure? v/fn)
        (hash-set i field (v/fn i))
        (hash-set i field v/fn))))

(provide/contract [table-remove-column (([table (apply table/c fields)]) () #:rest fields list? . ->d . [result table?])])
(define (table-remove-column table . fields)
  (for/list ([i table]) (for/fold ([result i]) ([f fields]) (hash-remove result f))))

(provide/contract [table-fill-missing (table? any/c (or (negate procedure?) (item? . -> . any)) . -> . table?)])
(define (table-fill-missing table field v/fn)
  (for/list ([i table])
    (cond [(hash-has-key? i field) i]
          [(procedure? v/fn) (hash-set i field (v/fn i))]
          [else (hash-set i field v/fn)])))

(provide/contract [table-update (([table (table/c field)] [field any/c] [v/fn (or (negate procedure?) (item? . -> . any))]) () . ->d . [result table?])])
(define (table-update table field v/fn)
  (map (// hash-update <> field v/fn) table))

(provide/contract [table-rename-column (([table (table/c old-name)] [old-name any/c] [new-name any/c]) () . ->d . [result table?])])
(define (table-rename-column table old-name new-name)
  (for/list ([i table])
    (hash-set (hash-remove i old-name)
              new-name (hash-ref i old-name))))

(provide/contract [column->list (([table (table/c field)] [field any/c]) () . ->d . [result list?])])
(define (column->list table field)
  (map (lambda (i) (.. i field)) table))

(define unique (procedure-rename remove-duplicates 'unique))
