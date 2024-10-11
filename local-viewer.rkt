#lang racket

(require "action.rkt" "connection.rkt" "setup.rkt" "viewer.rkt" "wandering.rkt")

(let* ([world (setup-world)]
       [connection (connect-local world)]
       [to-do (map wandering-action (send-hello connection))])
  (setup-blocks world)

  (define (draw-procedure draw-entity)
    (for ([entity (send-draw connection)])
      (apply draw-entity entity)))

  (define (do-actions)
    (set! to-do (perform-actions connection to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
