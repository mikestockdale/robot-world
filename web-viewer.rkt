#lang racket

(require net/http-client)
(require "viewer.rkt")

(define (draw-procedure draw-entity)

  (define-values (status headers in)
    (http-sendrecv "localhost"
                   "/draw"
                   #:port 8080
                   #:version "1.1"
                   #:method "GET"
                   #:data ""))
  (let ([entities (with-input-from-string (port->string in) read)])
    (for ([entity entities]) (apply draw-entity entity)))
  (close-input-port in))

(define (do-actions) #t )
  
(run-viewer "robots - localhost:8080" draw-procedure do-actions)
