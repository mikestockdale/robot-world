#lang racket

(require "action.rkt" "connection.rkt" "server/setup.rkt" "viewer.rkt" "gathering.rkt")

(let* ([engine (setup-engine)]
       [draw-connection (connect-local engine)]
       [action-connection (connect-local engine)]
       [to-do (gathering-actions (send-hello action-connection))])
  (setup-blocks engine)

  (define (draw-procedure draw-entity)
    (for ([entity (send-draw draw-connection)])
      (apply draw-entity entity)))

  (define (do-actions)
    (set! to-do (perform-actions action-connection to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
