#lang racket

(require "action.rkt" "connection.rkt" "setup.rkt" "viewer.rkt" "gathering.rkt")

(let* ([engine (setup-engine)]
       [connection (connect-local engine)]
       [to-do (gathering-actions (send-hello connection))])
  (setup-blocks engine)

  (define (draw-procedure draw-entity)
    (for ([entity (send-draw connection)])
      (apply draw-entity entity)))

  (define (do-actions)
    (set! to-do (perform-actions connection to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
