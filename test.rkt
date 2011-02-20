#lang racket

(provide (rename-out [check-match* check-match])
         check-exn-msg
         check-contract-exn
         check-contract-pass
         test
         test-data
         (all-from-out (planet schematics/schemeunit:3:4)))

(require
 (for-syntax "util.rkt")
 unstable/function
 (planet schematics/schemeunit:3:4)
 (planet schematics/schemeunit:3:4/text-ui)
 (planet schematics/schemeunit:3:4/format)
 (planet schematics/schemeunit:3:4/check))

(require (for-syntax unstable/syntax))

(define-check (check-match result input-pattern thunk)
  (with-check-info
      [('actual result)
       ('pattern input-pattern)]
    (unless (thunk result) (fail-check))))

;; Check that the result from evaluating ACTUAL can be matched against PATERN.
;; If not, the check fails.
(define-syntax (check-match* stx)
  (syntax-case stx ()
    [(_ input-actual input-pattern)
     (with-syntax
         ([no-loc (datum->syntax
                   stx
                   (syntax->datum
                    #'(lambda (v) (match v [input-pattern #t] [_ #f]))))])
       (syntax/loc stx (check-match input-actual 'input-pattern no-loc)))]))

(define-check (check-exn-msg exn-pred exn-regexp thunk)
  (let/ec return
    (with-check-info (('exp-pred exn-pred)
                      ((if (string? exn-regexp) 'match-string 'match-regexp) exn-regexp))
      (with-handlers
          ([(lambda (e) #t)
            (lambda (e)
              (with-check-info (('exn e)
                                ('exn-msg (and (exn? e) (exn-message e))))
                (unless (exn-pred e)
                  (with-check-info (('message "Wrong exception raised"))
                    (fail-check)))
                (unless (regexp-match? (if (string? exn-regexp)
                                           (regexp-quote exn-regexp)
                                           exn-regexp)
                                       (exn-message e))
                  (with-check-info (('message "Message doesn't match"))
                    (fail-check)))
                (return (void))))])
        (thunk))
      (with-check-info (('message "No exception raised")) (fail-check)))))

(define-check (check-contract-exn exn-regexp c v access-fn)
  (with-check-info (('contract c)
                    ('value v))
    (check-exn-msg exn:fail:contract? exn-regexp (lambda () ((or access-fn identity) (contract c v '|positive position| '|negative position|))))))

(define-check (check-contract-pass c v access-fn)
  (with-check-info (('contract c)
                    ('value v))
    (check-not-exn ((or access-fn identity) (lambda () (contract c v '|positive position| '|negative position|))))))

(provide/contract [current-test-on? (parameter/c (or/c boolean? symbol? string? (listof (or/c symbol? string?))))])
(define current-test-on? (make-parameter #t))

(current-check-handler
 (let ([prev (current-check-handler)])
   (lambda (e) (prev e) (raise e))))

(define (test-this? label)
  (or (eq? #t (current-test-on?))
      (equal? label (current-test-on?))
      (and (list? (current-test-on?)) (member label (current-test-on?)))))

(define-syntax (test-data stx)
  (syntax-case stx (define)
    [(_ (define v exp) ...)
     #;#'(begin (define v (if (void) exp #f)) ...)
     
     #'(begin (define v (if (current-test-on?) exp #f)) ...)]))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ label exp ...)
     (match? (syntax->datum #'label)
             (or (? string?) (list 'quote (? symbol?))))
     #'(define _
         (let ([label-v label])
           (when (test-this? label-v)
             (printf "testing ~a ... " label-v)
             (let () exp ...)
             (printf "ok~n"))))]
    [(_ exp ...) (syntax/loc stx (test "" exp ...))]))






