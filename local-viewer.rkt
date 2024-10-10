#lang racket

(require "action.rkt" "connection.rkt" "entity.rkt"
         "location.rkt" "viewer.rkt" "wandering.rkt" "world.rkt")

(let* ([world (make-world 50)]
       [connection (connect-local world)]
       [to-do (map wandering-action (send-hello connection))])
  (for ([x 5])
    (for ([y 5])
      (add-entity! world type-block
                   (location (+ 5 (* x 10)) (+ 5 (* y 10))))))

  (define (draw-procedure draw-entity)
    (draw-entities world draw-entity))

  (define (do-actions)
    (set! to-do (perform-actions connection to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
