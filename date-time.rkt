#lang racket
(require scheme/date 
         "cut.rkt")

(provide prefab->date)
(define (prefab->date d)
  (apply make-date (rest (vector->list (struct->vector d)))))

(define date-unit/c (symbols 'seconds 'minutes 'hours 'days))

(provide/contract [date-minus (date? date? date-unit/c . -> . number?)])
(define (date-minus a b unit)
  (/ (- (date->seconds a)
        (date->seconds b))
     (unit->seconds unit)))

(provide/contract [date<? (date? date? . -> . boolean?)])
(define (date<? a b)
  (< (date->seconds a) (date->seconds b)))

(provide/contract [unit->seconds (date-unit/c . -> . number?)])
(define (unit->seconds unit)
  (hash-ref #hash((seconds . 1)
                  (minutes . 60)
                  (hours . 3600)
                  (days . 86400))
            unit))

(provide/contract [convert-time (number? date-unit/c date-unit/c . -> . number?)])
(define (convert-time t from-unit to-unit)
  (/ (* t (unit->seconds from-unit))
     (unit->seconds to-unit)))

