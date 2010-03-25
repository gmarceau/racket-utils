#lang scheme
(provide (all-defined-out))

(define (read-printable? v)
  (or (prefab-struct-key v)
      (and (not (path-for-some-system? v))
           (not (struct? v)))))

(define (size-hook v like-display? port)
  (if (read-printable? v) #f 1))

(define (struct->prefab v)
  (apply make-prefab-struct (vector->list (struct->vector v))))

(define (print-hook v like-display? port)
  (define vv
    (cond [(path-for-some-system? v) (make-prefab-struct 'path (system-type 'os) (path->bytes v))]
          [(struct? v) (struct->prefab v)]))
  
  (parameterize ([pretty-print-columns 'infinity])
    ((if like-display? pretty-display pretty-print) vv port)))


(define (pretty-print-for-read data)
  (parameterize ([pretty-print-size-hook size-hook]
                 [pretty-print-print-hook print-hook])
    (parameterize ([pretty-print-columns 120])
      (pretty-print data)
      (newline))))

(define (log-result run-log-filename result)
  (with-output-to-file run-log-filename #:exists 'append
    (lambda ()
      (pretty-print-for-read result)))
  result)

