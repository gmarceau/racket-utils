#lang racket
(provide save-to-svg save-to-png)
(require slideshow/pict racket/draw)

(define (save-to-svg pict filename)
  (define my-dc (new svg-dc% [width (pict-width pict)] [height (pict-height pict)] [output filename] [exists 'replace]))
  (send my-dc start-doc "")
  (send my-dc start-page)
  (draw-pict pict my-dc 0 0)
  (send my-dc end-page)
  (send my-dc end-doc))

(define (save-to-png pict filename)
  (define b (make-bitmap (round (inexact->exact (pict-width pict))) (round (inexact->exact (pict-height pict)))))
  (define my-dc (new bitmap-dc% [bitmap b]))
  (draw-pict pict my-dc 0 0)
  (send b save-file filename 'png))