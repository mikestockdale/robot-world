#lang racket

(require "action.rkt" "entity.rkt" "server.rkt"
         "world.rkt" "location.rkt" "viewer.rkt" "wandering.rkt")

(let* ([world (make-world 50)]
       [server (connect-local world)]
       [to-do (map wandering-action (hello server))])
  (for ([x 5])
    (for ([y 5])
      (add-entity! world type-block
                   (location (+ 5 (* x 10)) (+ 5 (* y 10))))))

  (define (draw-procedure draw-entity)
    (draw-entities world draw-entity))

  (define (do-actions)
    (set! to-do (perform-actions server to-do)))
  
  (run-viewer "robots - local" draw-procedure do-actions))
