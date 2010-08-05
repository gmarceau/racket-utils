#lang racket

(provide (all-defined-out)
         (all-from-out (planet schematics/schemeunit:3:4)))

(require
 (planet schematics/schemeunit:3:4)
 (planet schematics/schemeunit:3:4/text-ui)
 (planet schematics/schemeunit:3:4/format)
 (planet schematics/schemeunit:3:4/check))

(require (for-syntax unstable/syntax))

;; (check-match actual patern)
;; Check that the result from evaluating ACTUAL can be matched against PATERN.
;; If not, the check fails.
(define-syntax (check-match stx)
  (syntax-case stx ()
    [(_ input-actual input-patern)
     (let ()
       (with-syntax
           ([no-loc (datum->syntax
                  stx
                  (syntax->datum
                   #'(lambda (result)
                       (check-true (match result [input-patern #t] [_ #f])))))])
        (syntax/loc stx
          (let ([result input-actual])
            (with-check-info
             [('actual result)
              ('patern 'input-patern)]
             (no-loc result))))))]))

(define current-test-on? (make-parameter #t))

(current-check-handler
 (let ([prev (current-check-handler)])
   (lambda (e) (prev e) (raise e))))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ str exp ...)
     (string? (syntax-e #'str))
     #'(when (current-test-on?)
         (printf "testing ~a ... " str)
         (let () exp ...))]
    [(_ exp ...) (syntax/loc stx (test "" exp ...))]))


;; run-tests/exn (or test-case test-suite) #:multiple boolean? -> void
;; Run all the given tests, and highlight the first one that fails.
;; If MULTIPLE? is false (which is the default,) testing stops after the first failure.
(define (run-tests/exn tests #:multiple? [multiple? #f])
  (define something-printed? #f)
  
  (define count
    (fold-test-results
     (lambda (result count)
       (define (throw-the-exception)
         (match result
           [(? test-failure?) (raise (test-failure-result result))]
           [(? test-error?) (raise (test-error-result result))]))
       
       (cond
         [(test-success? result) (pretty-print 'ok) count]
         [something-printed? (display-context result) (add1 count)]
         [multiple?
          (display-context result)
          (let ()
            (define t (thread throw-the-exception))
            (thread-wait t)
            (set! something-printed? #t)
            (add1 count))]
         [else (display-context result)
               (throw-the-exception)]))
     0 tests))
  (if (= 0 count)
      (printf "All tests passed!~n")
      (printf "~a tests failed.~n" count)))



