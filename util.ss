#lang scheme

(provide format-percent id average pad match? sequence)

(define (format-percent n) (format "~a%" (truncate (* 100 n))))

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

(define (average . args) (/ (apply + args) (length args)))

(define (pad length c v)
  (define str (format "~a" v))
  (format "~a~a" (make-string (max 0 (- length (string-length str))) c) str))

(define-syntax (match? stx)
  (syntax-case stx ()
    [(_ v pattern) (syntax/loc stx
                     (match v [pattern #t] [_ #f]))]))

(define (sequence init . fns)
  (for/fold ([result init]) ([fn fns])
    (fn result)))