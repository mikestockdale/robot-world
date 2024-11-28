#lang racket

(require "shared.rkt" "connection.rkt" "viewer.rkt")

;@title{TCP Viewer}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/tcp-viewer.rkt" "tcp-viewer.rkt"]}
;The TCP viewer connects to a TCP server and opens a viewer window..

(define (do-actions) #t )
  
(define (run)
  (let ([connection (connect-remote "localhost" 8080)])
    (define (draw-procedure draw-entity)
      (for ([entity (connection request-draw)])
        (apply draw-entity entity)))
  (viewer "robots - localhost:8080" draw-procedure do-actions)))

(run)
  