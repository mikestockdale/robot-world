#lang racket

(require "action.rkt" "connection.rkt" "setup.rkt" "viewer.rkt" "wandering.rkt" "world.rkt")

(let* ([world (setup-world)]
       [connection (connect-local world)]
       [to-do (map wandering-action (send-hello connection))])
  (setup-blocks world)

  (define (draw-procedure draw-entity)
    (draw-entities world draw-entity))

  (define (do-actions)
    (set! to-do (perform-actions connection to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
