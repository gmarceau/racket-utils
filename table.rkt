#lang racket

#|
This file implements a data structures called 'table'.

A table is a list of hash tables, where all the hash tables have the same keys. The keys are usually symbols, though
that is not necessary. The table data structure is meant to be reminiscent of a database table, with data rows and
labeled column.

The table.rkt module defines the contract for tables, and for the items found in a table:

;; (item/c KEYS ...) : an item which has the keys KEYS
;; (table/c KEYS ...) : a table whose items all have the keys KEYS

The table/c contract only requires that the given fields are present. It is allowed for items in the table to have
additional fields.

One of table.rkt’s most useful function is select, which mimics SQL’s select statements. Select takes a table as the
first argument, followed by alternating arguments, one field name followed by one constraint on that field. The
constraint can be either a value, or a predicate. Select returns the item which satisfy all the constraints. If the
optional argument #:field is given, select returns a list containing that field’s value for all the matching items. If
the optional arguments #:fields (note the ‘s’) is given, select returns the list of the matching items with only the
keys listed in #:fields kept.

For example, given a table of item with their names and their price, we can select those which costs 5$, select just
their name, or select all items with an even price.

    (define data '(#hash((name . "ball") (price . 5))
                   #hash((name . "bicycle") (price . 70))
                   #hash((name . "pool") (price . 100))
                   #hash((name . "batman") (price . 5))))

    (select data 'price 5)
    ; returns '(#hash((name . "ball") (price . 5)) #hash((name . "batman") (price . 5)))

    (select data 'price 5 #:field 'name)
    ; returns '("ball" "batman")

    (select data 'price even?)
    ; returns '(#hash((name . "bicycle") (price . 70)) #hash((name . "pool") (price . 100)))

By default, select returns the items in the same order as their appeared in the input table. If needed, Select has
optional arguments to indicate the desired sort. #:sort indicated the field to sort on, #:sort-fn specifies the
comparison function (mandatory if #:sort is given.) #:sort-key is a function that takes a field value and returns the
value suitable for the comparison function. If #:sort-cache-keys? is #t then the #:sort-key function is called only once
per field value.

Example: to get the price of the items with even price, starting with those with the shorter names:

    (select data 'price even?
            #:sort 'name #:sort-fn < #:sort-key string-length #:sort-cache-keys? #t
            #:field 'price)
    ; returns '(100 70)


Other useful functions in table.rkt are:

(find table #:field [field none-given]
      #:fields [fields #f]
      #:default [default none-given] . when-specs)

Like select, but checks that only one result is found, then returns that item

(table-group-by table . fields)

Takes a table and a field, and returns a hash table where the keys are all the possible values of the field in the
table, and the values are all the items that have that value for that field.

(table-index-by table . fields)

Like table-group-by, but assumes that each field value is unique. Thus the values of the hash table returned are items
instead of lists.

(join-on left-table right-table left-field
         #:right-field [right-field left-field]
         #:missing [missing 'error]
         #:duplicate [duplicate 'error])

Takes two tables (the left and the right table) and a field they have in common, and returns a table whose items have
all the fields of the two input table combined, and the field values are taken from the left item and the right item
which have the same value for the given field.

(column table field)

Returns all the values of the given field, in the same order as in the input table.

Since code that uses table.rkt tend to do numerous hash table accesses, table.rkt provides short names for the hash
table access functions. They are similar to the following definitions:

(define .. hash-get) ;; two period
(define !! hash-set)
(define -- hash-remove) ;; two hyphens
(define ?? hash-has-key?)

The accessor functions provided by table.rkt have some additional functionality, namely, they accept a variable number
of fields as arguments. When given two or more fields, dot-dot accesses nested hash tables, bang-bang sets all the given
key-value pairs, minus-minus removes all the given field, uh-uh returns #t only if all the given field of present.


|#

(require
 (planet neil/csv)
 (planet dherman/csv-write/csv-write)
 "test.rkt"
 "cut.rkt"
 "hash.rkt"
 "counting.rkt"
 "util.rkt"
 (only-in "list.rkt" group-pairwise)
 (only-in srfi/1 lset-difference zip unzip2))

(provide !! -- ??
         item? item/c table? table/c
         csv->table table->csv lstlst->table unique)

; a table is a (listof (hash symbol any))

(current-test-on? #f)

(define (item/c . keys) (rename-contract (and/c hash? (lambda (v) (andmap (// hash-has-key? v <>) keys))) (cons 'item/c keys)))

(define (extend-*/c being-extended-name being-extended-fn ic . keys)
  (define name (contract-name ic))
  (unless (match name [(list name k ...) (eq? name being-extended-name)] [_ false])
    (error 'extend-item/c "not an ~a contract:" ic))
  (rename-contract (and/c ic (apply being-extended-fn keys))
                   (cons being-extended-name (append (rest name) keys))))

(provide/contract [extend-item/c ((contract?) () #:rest list? . ->* . contract?)])
(define (extend-item/c ic . keys)
  (apply extend-*/c 'item/c item/c ic keys))

(define (extend-item-kv/c ic . keys)
  (apply extend-*/c 'item-kv/c item-kv/c ic keys))

(define (extend-table/c tc . keys)
  (apply extend-*/c 'table/c table/c tc keys))

(define (extend-table-kv/c tc . keys)
  (apply extend-*/c 'table-kv/c table-kv/c tc keys))

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


(test-data
 (define test-c (item-kv/c 1 number? 2 (symbols 'a 'b) 3 (any/c . -> . any/c)))
 (define test-c2 (item-kv/c 1 (number? number? . -> . number?))))

(test 'item/c* 0
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

(test 'extend-*/c (check-equal? (contract-name (extend-item/c (item/c 'x) 'y 'z))
                                '(item/c x y z))
      (check-contract-exn "(item/c x y z)" (extend-item/c (item/c 'x) 'y 'z) (hash 'x 1) #f)
      (check-contract-exn "(item/c x y z)" (extend-item/c (item/c 'x) 'y 'z) (hash 'y 1 'z 2) #f)
      (check-contract-pass (extend-item/c (item/c 'x) 'y 'z) (hash 'x 1 'y 1 'z 2) #f)
      
      (check-equal? (contract-name (extend-item-kv/c (item-kv/c 'x number?) 'y string? 'z boolean?))
                    `(item-kv/c x ,number? y ,string? z ,boolean?))
      (check-equal? (contract-name (extend-table/c (table/c 'x) 'y 'z))
                    '(table/c x y z))
      (check-equal? (contract-name (extend-table-kv/c (table-kv/c 'x number?) 'y string? 'z boolean?))
                    `(table-kv/c x ,number? y ,string? z ,boolean?))
      
      (check-contract-exn "(table/c x y z)" (extend-table/c (table/c 'x) 'y 'z) (list (hash 'x 1)) #f)
      (check-contract-pass (extend-table/c (table/c 'x) 'y 'z) (list (hash 'x 1 'y 1 'z 2)) #f))

(define !! hash-set-all)

(define (-- hash . fields) (for/fold ([result hash]) ([f fields]) (hash-remove result f)))

(define (?? hash . fields) (for/and ([f fields]) (hash-has-key? hash f)))

(define none-given (gensym))

(define default-proc/c (or/c (negate procedure?) (-> any)))

(provide/contract [.. (([hash hash?]) (#:default [default default-proc/c]) #:rest rst any/c . ->d . [result any/c])])
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

(provide/contract [csv-field-order (path-string? . -> . (listof symbol?))])
(define (csv-field-order filename) (map string->symbol (first (csv->list (with-input-from-file filename read-line)))))

(define (lstlst->table field-names lstlst)
  (for/list ([lst lstlst])
    (make-immutable-hash
     (map cons field-names lst))))

;; WHEN-SPEC is (listof (list/c field-name (or/c value (any . -> . boolean?)))
(define predicate-like/c any/c)
(define comparison-like/c any/c)
(define when-spec-contract (list-pairwise/c any/c (or/c (negate procedure?) predicate-like/c)))
(define select-contract (([table table?])
                         (#:field [field any/c] #:fields [fields list?] #:sort [sort-field any/c]
                                  #:sort-fn [sort-fn comparison-like/c] #:sort-key [sort-key (any/c . -> . any)]
                                  #:sort-cache-keys? [sort-cache-keys? boolean?])
                         #:rest rst when-spec-contract
                         . ->d . [result list?]))
(provide/contract [select select-contract])
(define (select table #:field [field none-given] #:fields [fields #f]
                #:sort [sort-field none-given]
                #:sort-fn [sort-fn <]
                #:sort-key [sort-key (lambda (x) x)]
                #:sort-cache-keys? [sort-cache-keys? #f]
                . when-specs)
  
  (define when-specs-grouped (group-pairwise when-specs))
  
  (define (check-has-field i f)
    (unless (hash-has-key? i f)
      (error 'select (format "field `~a' is missing in: ~a" f i))))
  
  (define (check-has-fields table fields)
    (for* ([i table]
           [f fields])
      (check-has-field i f)))
  
  (define (matches? i)
    (andmap
     (match-lambda [(list f (? procedure? fn)) (check-has-field i f) (fn (hash-ref i f))]
                   [(list f v) (check-has-field i f) (equal? (hash-ref i f) v)])
     when-specs-grouped))
  
  (when (and (not (eq? field none-given)) fields)
    (error 'select "only one of #:field or #:fields is allowed"))
  
  (define targetted (filter matches? table))
  
  (define post-needed-fields (append (cond [(not (eq? field none-given)) (list field)]
                                           [fields fields]
                                           [else empty])
                                     (if (not (eq? sort-field none-given)) (list sort-field) empty)))
  
  (check-has-fields targetted post-needed-fields)
  
  (define sorted (if (not (eq? sort-field none-given))
                     (sort targetted sort-fn #:key (lambda (v) (sort-key (.. v sort-field))) #:cache-keys? sort-cache-keys?)
                     targetted))
  
  (for/list ([i sorted])
    (cond [(not (eq? field none-given)) (hash-ref i field)]
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
(provide/contract [find (([table table?]) (#:field [field any/c] #:fields [fields list?] #:default [default default-proc/c]) #:rest rst when-spec-contract . ->d . [result any/c])])
(define (find table #:field [field none-given] #:fields [fields #f] #:default [default none-given] . when-specs)
  (match (apply select table #:field field #:fields fields when-specs)
    [(list)
     (cond [(eq? default none-given) (error 'find "nothing found: ~a" when-specs)]
           [(procedure? default) (default)]
           [else default])]
    [(list v) v]
    [many (error 'find "more than one found: ~a" many)]))

(provide/contract [table-index-by (([table (table/c field)] [field any/c]) () . ->d . [result (hash/c any/c item?)])])
(define (table-index-by table field)
  (define result (for/list ([i table]) (cons (hash-ref i field) i)))
  (define dup (check-duplicates result #:key car))
  (when dup (error 'table-index-by "duplicate index key: ~a" dup))
  (make-immutable-hash result))

(test 'table-index-by
      (check-equal? (table-index-by (list (hash 'a 1) (hash 'a 2)) 'a)
                    (hash 1 (hash 'a 1) 2 (hash 'a 2)))
      (check-exn-msg exn:fail? "duplicate" (lambda () (table-index-by (list (hash 'a 1) (hash 'a 1)) 'a))))

(provide/contract [table-sort-by (table? any/c comparison-like/c . -> . table?)])
(define (table-sort-by table field less-than #:key [key-fn identity] #:cache-keys? [cache-keys? #f])
  (sort table less-than #:key (lambda (i) (key-fn (hash-ref i field))) #:cache-keys? cache-keys?))

(define table-group-by-contract (([table (apply table/c rst)]) () #:rest rst any/c . ->d . [result (hash/c any/c (apply table/c rst))]))
(provide/contract [table-group-by table-group-by-contract])
(define (table-group-by table . fields)
  (define result (for/fold ([result empty-hash]) ([i table])
                   (define k (match fields [(list f) (hash-ref i f)]
                               [_ (map (// hash-ref i <>) fields)]))
                   (hash-update result k (// cons i <>) empty)))
  (hash-map-values:h result reverse))

(test 'table-group-by
      (check-equal? (table-group-by (cons (hash 'x 4) data) 'x)
                    (hash 1 (list (hash 'x 1 'y 2 'z 3 'boo "foo"))
                          4 (list (hash 'x 4) (hash 'x 4 'y 5 'z 6))))
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
                          [(or (and (eq? left-field right-field) (eq? key right-field))
                               (eq? duplicate 'left))
                           result]
                          [(eq? duplicate 'error)
                           (error 'join-on "the joined tables duplicate field: ~a" key)]))
                  
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
                                            #:destination [destination-field any/c]
                                            #:missing [missing (symbols 'partial 'remove 'error)])
                             . ->d . [result (table-kv/c left-field (item/c (or-supply right-field left-field)))]))
(provide/contract [join-nested join-nested-contact])
(define (join-nested left-table right-table left-field
                     #:right-field [right-field left-field]
                     #:destination [destination-field left-field]
                     #:missing [missing 'error])
  (define indexed (table-index-by right-table right-field))
  (filter (lambda (i) i)
          (for/list ([i left-table])
            (let ([left-val (hash-ref i left-field)])
              (if (hash-has-key? indexed left-val)
                  (hash-set i destination-field (hash-ref indexed left-val))
                  (match missing
                    ['partial i]
                    ['remove #f]
                    ['error (error 'join-on "join target doesn't exists in the right table: ~a" left-val)]))))))

(test 'join-nested
      (check-equal? (join-nested test-join-left test-join-right 'b #:right-field 'c)
                    (list (hash 'a 1 'b (hash 'c 2 'd 10))
                          (hash 'a 4 'b (hash 'c 5 'd 15))))
      (check-equal? (join-nested test-join-left test-join-right 'b #:right-field 'c #:destination 'd)
                    (list (hash 'a 1 'b 2 'd (hash 'c 2 'd 10))
                          (hash 'a 4 'b 5 'd (hash 'c 5 'd 15)))))

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
                   (list (hash-table ('a 1) ('b (list (hash-table ('c 2) ('d 10)))))
                         (hash-table ('a 4) ('b (list-no-order (hash-table ('c 5) ('d 20)) (hash-table ('c 5) ('d 15))))))))

(provide/contract [table-add-column (([table table?] [field any/c] [v/fn (or/c (negate procedure?) (item? . -> . any))])
                                     () . ->d . [result (table/c field)])])
(define (table-add-column table field v/fn)
  (for/list ([i table])
    (if (procedure? v/fn)
        (hash-set i field (v/fn i))
        (hash-set i field v/fn))))

(provide/contract [table-remove-column (([table (apply table/c fields)]) () #:rest fields any/c . ->d . [result table?])])
(define (table-remove-column table . fields)
  (for/list ([i table]) (for/fold ([result i]) ([f fields]) (hash-remove result f))))

(provide/contract [table-fill-missing (table? any/c (or/c (negate procedure?) (item? . -> . any)) . -> . table?)])
(define (table-fill-missing table field v/fn)
  (for/list ([i table])
    (cond [(hash-has-key? i field) i]
          [(procedure? v/fn) (hash-set i field (v/fn i))]
          [else (hash-set i field v/fn)])))

(provide/contract [table-update (([table (table/c field)] [field any/c] [v/fn (or/c (negate procedure?) (any/c . -> . any))]) () . ->d . [result table?])])
(define (table-update table field v/fn)
  (map (// hash-update <> field v/fn) table))

(provide/contract [table-rename-column (([table (table/c old-name)] [old-name any/c] [new-name any/c]) () . ->d . [result table?])])
(define (table-rename-column table old-name new-name)
  (for/list ([i table])
    (hash-set (hash-remove i old-name)
              new-name (hash-ref i old-name))))

(provide/contract [column (([table (table/c field)] [field any/c]) () . ->d . [result list?])])
(define (column table field)
  (map (lambda (i) (.. i field)) table))

(define unique (procedure-rename remove-duplicates 'unique))

; item-select: Takes an item and a list of fields, and returns an items with only those fields.
(provide/contract [item-select (([item (apply item/c fields)]) () #:rest fields any/c . ->d . [result (apply item/c fields)])])
(define (item-select item . fields)
  (for/fold ([result empty-hash]) ([f fields])
    (hash-set result f (hash-ref item f))))

; item-join: takes two items returns an item that has the keys of both of them. Errors if any duplicate.
(provide/contract [item-join (item? item? . -> . item?)])
(define (item-join item1 item2)
  (for/fold ([result item1]) ([(k v) item2])
    (when (hash-has-key? item1 k) (error 'item-join "duplicate key: ~a" k))
    (hash-set result k v)))

;; table-fields: return all fields seen in the table, including those which are sometime missing
(provide/contract [table-fields (table? . -> . list?)])
(define (table-fields tbl) (unique (append-map hash-keys tbl)))

; hash->table: return a table with two columns (k-name and v-name). The k-name's are the key, and their v-name are the matching value in the hash.
(provide/contract [hash->table (([h hash?] [k-name any/c] [v-name any/c]) () . ->d . [result (table/c k-name v-name)])])
(define (hash->table h k-name v-name) (hash-map h (lambda (k v) (hash k-name k v-name v))))



