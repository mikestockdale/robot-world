#lang racket

(require "actions.rkt" "direction.rkt" "entity.rkt" "server.rkt"
         "world.rkt" "location.rkt" "viewer.rkt" "wandering.rkt")

(let* ([world (make-world 50)]
       [server (connect-local world)]
       [to-do
        (actions server
                 (list
                  (make-wandering server (location 20 20) direction-east)
                  (make-wandering server (location 30 30) direction-west)))])
  (add-entity! world type-block (location 25 25))
  (add-entity! world type-block (location 35 25))
  (add-entity! world type-block (location 25 35))
  (add-entity! world type-block (location 45 25))
  (add-entity! world type-block (location 25 45))
  (add-entity! world type-block (location 15 25))
  (add-entity! world type-block (location 25 15))
  (add-entity! world type-block (location 5 25))
  (add-entity! world type-block (location 25 5))

  (define (draw-procedure draw-entity)
    (draw-entities world draw-entity))

  (define (do-actions)
    (set! to-do (perform-actions to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
