#lang scheme

(require "cut.ss")

(provide/contract [path-string?
                   (any/c . -> . boolean?)])

;; List the content of the directory, building paths with build-path.
(provide/contract (directory-list*
                   (path-string? . -> . (listof path-string?))))
(define (directory-list* dirname)
  (map (// build-path dirname <>)
       (directory-list dirname)))

(provide/contract [directory-list/recursive
                   (path-string? . -> . (listof path-string?))])
(define (directory-list*/recursive dirname)
  (flatten
   (for/list ([item (directory-list* dirname)])
     (if (directory-exists? item)
         (list item (directory-list/recursive item))
         item))))

;; Returns the content of the file as parsed by READ, as a sequence.
(provide/contract [in-read-file
                   (path-string? . -> . sequence?)])
(define (in-read-file filename)
  (define p (open-input-file filename))
  (in-producer (lambda () (read p)) eof))

;; Return the part after the last dot.
(provide/contract [path-extension
                   (path-string? . -> . (or/c path? false/c))])
(define (path-extension p)
  (define-values (base name _) (split-path p))
  (define m (regexp-match #rx".*[.]([^.]*)$" (path->bytes name)))
  (and m (bytes->path (second m) (path-convention-type p))))

;; Return the part of the filename (without the directory part)
;; that is before the last dot.
(provide/contract [path-basename
                   (path-string? . -> . (or/c path? false/c))])
(define (path-basename p)
  (define-values (base name _) (split-path p))
  (define m (regexp-match #rx"(.*)[.][^.]*$" (path->bytes name)))
  (if m
      (bytes->path (second m) (path-convention-type p))
      (path-filename p)))

;; Returns the base part according to split-path.
(provide/contract [path-dirname
                   (path-string? . -> . (or/c path? false/c))])
(define (path-dirname p)
  (define-values (base name _) (split-path p))
  (and (path? base) base))

;; Return the name part according to split-path.
(provide/contract [path-filename
                   (path-string? . -> . (or/c path? false/c))])
(define (path-filename p)
  (define-values (base name _) (split-path p))
  (and (path? name) name))

