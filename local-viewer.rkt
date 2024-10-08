#lang racket

(require "action.rkt" "direction.rkt" "entity.rkt" "server.rkt"
         "world.rkt" "location.rkt" "viewer.rkt" "wandering.rkt")

(let* ([world (make-world 50)]
       [server (connect-local world)]
       [to-do
        (list
         (make-wandering server (location 10 10) direction-north)
         (make-wandering server (location 20 20) direction-east)
         (make-wandering server (location 30 30) direction-south)
         (make-wandering server (location 40 40) direction-west))])
  (for ([x 5])
    (for ([y 5])
      (add-entity! world type-block
                   (location (+ 5 (* x 10)) (+ 5 (* y 10))))))

  (define (draw-procedure draw-entity)
    (draw-entities world draw-entity))

  (define (do-actions)
    (set! to-do (perform-actions server to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
