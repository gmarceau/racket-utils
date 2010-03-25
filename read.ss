#lang scheme
(provide (all-defined-out))

(define (read-all port)
  (let ([v (read port)])
    (if (eof-object? v)
        empty
        (cons v (read-all port)))))

(define (read-all-syntax source-name port)
  (let ([v (read-syntax source-name port)])
    (if (eof-object? v)
        empty
        (cons v (read-all-syntax source-name port)))))

;; read-all-syntax/reader: port -> (listof syntax?)
(define (read-all-syntax/reader port)
  (port-count-lines! port)
  (parameterize ([read-accept-reader #t])
    (read-all-syntax 'program port)))