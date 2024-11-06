#lang racket

(require "connection.rkt" "viewer.rkt")

(define (do-actions) #t )
  
(define (run)
  (let ([connection (connect-remote "localhost" 8080)])
    (define (draw-procedure draw-entity)
      (for ([entity (send-draw connection)])
        (apply draw-entity entity)))
  (run-viewer "robots - localhost:8080" draw-procedure do-actions)))

(run)
  