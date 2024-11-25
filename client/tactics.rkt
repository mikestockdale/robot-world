#lang racket

(provide blocks-nearby? removable-blocks direction-from 
         (struct-out choice) direction-change-chance
         choose-drop choose-move choose-take)

(require "shared.rkt" "bot.rkt")
(module+ test (require rackunit))

;@title{Tactics}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/tactics.rkt" "tactics.rkt"]}
;This is a collection of functions for use in implementing an action strategy.

;The @bold{direction change chance} is the chance of making a random direction change when a bot is wandering around the world.
;This is set to a 20% chance.

(define direction-change-chance (make-parameter 0.2))

;The @bold{direction from} one location to another is a direction that will move a bot closer to a destination.

(test-case:
 "direction from location to location"
 (check-equal? (direction-from (location 1 1) (location 2 1)) direction-east)
 (check-equal? (direction-from (location 1 1) (location 3 4)) direction-north)
 (check-equal? (direction-from (location 1 1) (location 4 3)) direction-east)
 (check-equal? (direction-from (location 1 4) (location 3 1)) direction-south)
 (check-equal? (direction-from (location 4 1) (location 1 3)) direction-west))

;The direction returned will reduce the larger of the x and y difference.

(define (direction-from from to)
  (let-values ([(difference-x difference-y) (location-offset from to)])
    (if (> (abs difference-x) (abs difference-y))
        (if (positive? difference-x) direction-west direction-east)
        (if (positive? difference-y) direction-south direction-north))))

;There are @bold{blocks nearby} if they are in the bot's neighbor list.

(test-case:
 "blocks nearby"
 (check-true (blocks-nearby? (bot #f #f (list (entity 101 type-block #f)))))
 (check-false (blocks-nearby? (bot #f #f (list (entity 101 type-bot #f)))))
 (check-false (blocks-nearby? (bot #f #f '()))))

(define (blocks-nearby? bot)
  (ormap (λ (entity) (= (entity-type entity) type-block))
         (bot-neighbors bot)))

;The @bold{choice} structure is used to return information about what a strategy has chosen for the next action.

(struct choice (type parameter direction delay))

;When a strategy @bold{choose}s to @bold{drop} a block, the choice parameter is the direction to a free location.
;The next move direction is different from the drop direction, and the delay before taking another block is 5 turns.

(test-case:
 "free location found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block (entity 102 type-block (location 1 2))]
        [bot (bot bot1 #f (list block))]
        [choice (choose-drop bot)])
   (check-equal? (choice-type choice) request-drop)
   (check-equal? (choice-parameter choice) direction-east)
   (check-not-equal? (choice-direction choice) direction-east)
   (check-equal? (choice-delay choice) 5)))

;The direction chosen places the block in a location with the most adjacent blocks.

(test-case:
 "best location found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block1 (entity 102 type-block (location 0 0))]
        [block2 (entity 103 type-block (location 2 0))]
        [choice (choose-drop (bot bot1 #f (list block1 block2)))])
   (check-equal? (choice-parameter choice) direction-south)))

;The direction chosen doesn't move outside the world
    
(test-case:
 "free location not outside world"
 (let* ([bot1 (entity 101 type-bot (location 49 49))]
        [edge1 (make-edge (location 50 49))]
        [edge2 (make-edge (location 49 50))]
        [block (entity 102 type-block (location 49 48))]
        [choice (choose-drop (bot bot1 #f (list edge1 edge2 block)))])
   (check-equal? (choice-parameter choice) direction-west)))

;A location is free if there are no entities at the location.
;The chosen direction is the one to the location with most adjacent blocks

(define (choose-drop bot)
  (define (is-free? location)
    (not (findf (λ (neighbor) (equal? location (entity-location neighbor)))
                (bot-neighbors bot))))
  (define (score location)
    (if (is-free? location) (count-adjacent bot location) -1))
  (let* ([drop-location (argmax score (all-directions (bot-location bot)))]
         [drop-direction (direction-from (bot-location bot) drop-location)])
    (choice request-drop drop-direction
            (change-direction drop-direction) 5)))

(define (count-adjacent bot location)
  (define (adjacent-block? entity)
    (and (= (entity-type entity) type-block)
         (adjacent? (entity-location entity) location)))
  (count adjacent-block? (bot-neighbors bot)))

;When a strategy @bold{choose}s to @bold{move} a bot, the delay before taking another block is reduced by one.

(test-case:
 "delay reduced"
 (let ([choice (choose-move direction-west 5)])
   (check-equal? (choice-type choice) request-move)
   (check-equal? (choice-parameter choice) direction-west)
   (check-equal? (choice-direction choice) direction-west)
   (check-equal? (choice-delay choice) 4)))

(define (choose-move direction delay)
  (choice request-move direction direction (max 0 (- delay 1))))

;When a strategy @bold{choose}s to @bold{take} a block, the choice parameter is block id.
;The next move direction is the direction to the block, and the delay before taking another block is 5 turns.

(test-case:
 "choose take"
 (let ([choice (choose-take
                (bot (entity 101 type-bot (location 1 1)) #f #f)
                (entity 102 type-block (location 1 2)))])
   (check-equal? (choice-type choice) request-take)
   (check-equal? (choice-parameter choice) 102)
   (check-equal? (choice-direction choice) direction-north)
   (check-equal? (choice-delay choice) 5)))

(define (choose-take bot block)
  (let ([take-direction (direction-from (bot-location bot) (entity-location block))]) 
    (choice request-take (entity-id block) take-direction 5)))

;@bold{Removeable blocks} are ones that are adjacent to a bot and are not adjacent to two other blocks.

(test-case:
 "removable block found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block1 (entity 102 type-block (location 1 2))]
        [block2 (entity 102 type-block (location 2 2))]
        [bot2 (entity 103 type-bot (location 2 1))]
        [bot (bot bot1 #f (list bot2 block1 block2))]
        [removable (removable-blocks bot)])
   (check-equal? (length removable) 1)
   (check-equal? (first removable) block1)))
  
(test-case:
 "block not removable"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block1 (entity 102 type-block (location 1 2))]
        [block2 (entity 102 type-block (location 2 2))]
        [block3 (entity 103 type-block (location 0 2))]
        [bot (bot bot1 #f (list block1 block2 block3))]
        [removable (removable-blocks bot)])
   (check-equal? (length removable) 0)))

;We filter the bot's neighbors to find removable blocks

(define (removable-blocks bot)
  (filter (λ (entity)
            (and (= (entity-type entity) type-block)
                 (adjacent? (entity-location entity) (bot-location bot))
                 (< (count-adjacent bot (entity-location entity)) 2)))
          (bot-neighbors bot)))
