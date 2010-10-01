#lang racket

(require unstable/contract "contract.rkt")

(provide match? match?-lambda pipe lambda-pipe list-even-length/c list-pairwise/c)

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





