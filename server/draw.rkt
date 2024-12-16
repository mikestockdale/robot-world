#lang racket/base

(provide draw-entities)

(require racket/list racket/string)
(require "agent.rkt" "engine.rkt" "grid.rkt" "shared.rkt" "testing.rkt")
(module+ test (require rackunit))

;@title{Draw}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/draw.rkt" "draw.rkt"]}

;A draw request returns a list of information needed to @bold{draw entities}.

(test-case:
 "entities are drawn"
 (test-engine
  ((size 3 4) (bot1 0 2) (block 2 1) (bot2 1 1))
  (let ([player (make-agent)])
    (assign-bots! player (list (entity-id bot2)))
    (match-request player request-hello)
    (add-to-score player 123)
    (check-equal?
     (draw-entities (list player) (engine-grid engine))
     (list "123" (list type-bot #f #f 0 2) (list type-block #f #f 2 1) (list type-bot 0 #f 1 1))))))

;A draw reply is created from the player agents and a list of all entities from the grid.

(define (draw-entities players grid)
  (cons
   (if (empty? players)
       0
       (string-join
        (map (λ (agent) (number->string (agent-score agent))) players)))
   (map-entities
    grid
    (λ (occupant)
      (let ([entity (occupant-entity occupant)]
            [location (occupant-place occupant)])
        (and (at-location? location)
             (list (entity-type entity)
                   (find-agent players (entity-id entity))
                   (if (entity-at grid (entity-id entity)) #t #f)
                   (location-x location)
                   (location-y location))))))))
   
