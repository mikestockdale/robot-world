#lang racket

(provide (struct-out choice) direction-change-chance
         choose-move choose-take)

(require "shared.rkt" "bot.rkt")
(module+ test (require rackunit))

;@title{Tactics}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/tactics.rkt" "tactics.rkt"]}
;This is a collection of functions for use in implementing an action strategy.

;The @bold{direction change chance} is the chance of making a random direction change when a bot is wandering around the world.
;This is set to a 20% chance.

(define direction-change-chance (make-parameter 0.2))

;The @bold{choice} structure is used to return information about what a strategy has chosen for the next action.

(struct choice (type parameter direction))

;When a strategy @bold{choose}s to @bold{move} a bot, the choice parameter is the direction.

(test-case:
 "choose move"
 (let ([choice (choose-move direction-west)])
   (check-equal? (choice-type choice) request-move)
   (check-equal? (choice-parameter choice) direction-west)
   (check-equal? (choice-direction choice) direction-west)))

(define (choose-move direction)
  (choice request-move direction direction))

;When a strategy @bold{choose}s to @bold{take} a block, the choice parameter is block id.
;The next move direction is the direction to the block.

(test-case:
 "choose take"
 (let ([choice (choose-take
                (bot (entity 101 type-bot) (location 1 1) #f #f)
                (occupant (entity 102 type-block) (location 1 2)))])
   (check-equal? (choice-type choice) request-take)
   (check-equal? (choice-parameter choice) 102)
   (check-equal? (choice-direction choice) direction-north)))

(define (choose-take bot block)
  (let ([take-direction (direction-from (bot-location bot) (occupant-place block))]) 
    (choice request-take (entity-id (occupant-entity block)) take-direction)))
