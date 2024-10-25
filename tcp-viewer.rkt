#lang racket

(require net/http-client)
(require "connection.rkt" "viewer.rkt")

(define (draw-procedure draw-entity)
  (let ([connection (connect-remote "127.0.0.1" 8080)])
    (for ([entity (send-draw connection)])
      (apply draw-entity entity))))

(define (do-actions) #t )
  
(run-viewer "robots - localhost:8080" draw-procedure do-actions)
