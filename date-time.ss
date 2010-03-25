#lang scheme
(require scheme/date 
         gmarceau/cut)
(provide (all-defined-out))

(define (prefab->date d)
  (apply make-date (rest (vector->list (struct->vector d)))))

(define (date->seconds d)
  (find-seconds (date-second d) (date-minute d)
                (date-hour d) (date-day d) (date-month d)
                (date-year d)))

(define (date-minus a b unit)
  (/ (- (date->seconds a)
        (date->seconds b))
     (unit->seconds unit)))

(define (date<? a b)
  (< (date->seconds a) (date->seconds b)))

(define (unit->seconds unit)
  (hash-ref #hash((seconds . 1)
                  (minutes . 60)
                  (hours . 3600)
                  (days . 86400))
            unit))

(define (convert-time t from-unit to-unit)
  (/ (* t (unit->seconds from-unit))
     (unit->seconds to-unit)))

