#lang racket

(provide draw-entities)

(require "engine.rkt" "grid.rkt" "shared.rkt" "testing.rkt")
(module+ test (require rackunit))

;@title{Draw}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/draw.rkt" "draw.rkt"]}

;A draw request returns a list of information for drawing entities.

(test-case:
 "entities are drawn"
 (test-engine
  ((size 3) (bot1 0 2) (block 2 1) (bot2 1 1))
  (check-equal? (draw-entities (engine-grid engine))
                (list (list type-bot #f 0 2) (list type-block #f 2 1) (list type-bot #f 1 1)))))

;A draw reply is created from a list of all entities from the grid.

(define (draw-entities grid)
  (map-entities
   grid
   (λ (occupant)
     (let ([entity (occupant-entity occupant)]
            [location (occupant-place occupant)])
       (and (at-location? location) 
            (list (entity-type entity)
                  (if (entity-at grid (entity-id entity)) #t #f)
                  (location-x location)
                  (location-y location)))))))
   
