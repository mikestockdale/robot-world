#lang racket

(require "shared.rkt" "client/connection.rkt" "client/viewer.rkt")

(define (do-actions) #t )
  
(define (run)
  (let ([connection (connect-remote "localhost" 8080)])
    (define (draw-procedure draw-entity)
      (for ([entity (connection request-draw)])
        (apply draw-entity entity)))
  (viewer "robots - localhost:8080" draw-procedure do-actions)))

(run)
  