#lang racket

(provide id match? match?-lambda pipe lambda-pipe)

(provide/contract [format-percent (number? . -> . string?)])
(define (format-percent n) (format "~a%" (round (* 100 n))))

;; indent: Indent the string STR by n spaces. STR may have multiple lines
(provide/contract [indent (number? string? . -> . string?)])
(define (indent n str)
  (define indent-spaces (build-string n (lambda (i) #\space)))
  (string-join
   (for/list ([ln (in-lines (open-input-string str))])
     (format "~a~a" indent-spaces ln))
   (format "~n")))

(provide/contract [indent/bullet (any/c number? string? . -> . string?)])
(define (indent/bullet bullet n str)
  (define bullet-str (format "~a" bullet))
  (define bullet-len (string-length bullet-str))
  (format "~a ~a~n"
          bullet-str
          (substring (indent (+ bullet-len 1 n) str)
                     (add1 bullet-len))))

(define (id i) i)

(provide/contract [average (() () #:rest (listof number?) . ->* . number?)])
(define (average . args) (/ (apply + args) (length args)))

(provide/contract [pad (natural-number/c char? any/c . -> . string?)])
(define (pad length c v)
  (define str (format "~a" v))
  (format "~a~a" (make-string (max 0 (- length (string-length str))) c) str))

(define-syntax (match? stx)
  (syntax-case stx ()
    [(_ v pattern) (syntax/loc stx
                     (match v [pattern #t] [_ #f]))]))

(define-syntax (match?-lambda stx)
  (syntax-case stx ()
    [(_ pattern) (syntax/loc stx
                   (match-lambda [pattern #t] [_ #f]))]))


(require "cut.rkt")
(define-syntax (pipe stx)
  (syntax-case stx ()
    [(_ init (callee args ...) ...)
     (syntax/loc stx
       (for/fold ([result init]) ([fn (list (// callee args ...) ...)])
         (fn result)))]))

(define-syntax (lambda-pipe stx)
  (syntax-case stx ()
    [(_ (callee args ...) ...)
     (syntax/loc stx
       (lambda (init) (pipe init (callee args ...) ...)))]))

(provide list-even-length?/c)
(define list-even-length?/c (flat-named-contract
                             'list-even-length/c
                             (and/c list?
                                    (lambda (lst) (even? (length lst))))))

(provide/contract [group-pairwise (list-even-length?/c . -> . (listof (list/c any/c any/c)))])
(define/contract (group-pairwise lst)
  (list-even-length?/c . -> . (listof (list/c any/c any/c)))
  (match lst
    [(list) empty]
    [(list fst snd rst ...)
     (cons (list fst snd) (group-pairwise rst))]
    [else (error 'group-pairwise "there is a key that does not have a value")]))

#;
(require (for-syntax
          racket/match
          racket/list))
#;
(define-match-expander (h stx)
  (let ()
    (define (group-pair-wise lst)
      (match lst
        [(list) empty]
        [(list fst snd rst ...)
         (cons (list fst snd) (group-pair-wise rst))]
        [else (raise-syntax-error #f "there is a key that does not have a match value" stx)]))
    (printf "hash~n")
    (syntax-case stx ()
      [(_ arg ...)
       (begin
         (printf "~a~n" (group-pair-wise (syntax->list #'(arg ...))))
         (with-syntax ([(grouped ...) (group-pair-wise (syntax->list #'(arg ...)))])
           (syntax/loc stx (hash-table grouped ...))))
       ])))

