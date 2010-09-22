#lang racket

(provide/contract [read-all (() (port?) . ->* . list?)])
(define (read-all [port (current-input-port)])
  (let ([v (read port)])
    (if (eof-object? v)
        empty
        (cons v (read-all port)))))

(provide/contract [read-all-syntax (() (any/c port?) . ->* . (listof syntax?))])
(define (read-all-syntax [source-name #f] [port (current-input-port)])
  (let ([v (read-syntax source-name port)])
    (if (eof-object? v)
        empty
        (cons v (read-all-syntax source-name port)))))

;; read-all-syntax/reader: port -> (listof syntax?)
(provide/contract [read-all-syntax/reader (() (any/c port?) . ->* . (listof syntax?))])
(define (read-all-syntax/reader [source #f] [port (current-input-port)])
  (port-count-lines! port)
  (parameterize ([read-accept-reader #t])
    (read-all-syntax source port)))