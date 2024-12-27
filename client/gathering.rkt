#lang racket/base

(provide gathering-actions)

(require racket/list)
(require "shared.rkt" "action.rkt" "bot.rkt" "tactics.rkt")
(module+ test (require rackunit))

;@title{Gathering}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/gathering.rkt" "gathering.rkt"]}
;Gathering is a modification of the wandering strategy.
;The bots start adjacent to a base.
;The bots still wander randomly, taking blocks when they find them, but they bring them to their starting location to transfer them to the base.
;It's the first strategy to accomplish a basic goal.

;The strategy keeps track of the current direction each bot is facing.
;It has a destination location, where the blocks are transfered.

(struct gathering (direction destination))

;At the start of the game, a list of actions is generated from the list of bots assigned to the client.

(define (gathering-actions replies)
  (map (λ (reply)
         (let ([bot (make-bot reply)])
           (action (gather (gathering direction-east (bot-location bot)))
                   #f #f #t bot)))
       replies))

;At each turn, a choice is made for each bot and the action is updated. 

(define ((gather spec) input-action)
  (let ([choice (choose spec input-action)])
    (values
     (gather (struct-copy
              gathering spec
              [direction (choice-direction choice)]))
     (request (choice-type choice)
              (bot-id (action-bot input-action))
              (choice-parameter choice)))))

;A couple of helper methods for testing

(module+ test
  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action #f command #f success
            (bot (entity 101 type-bot) (location 1 1) cargo neighbors))))

(module+ test
  (define (gather-with
           #:chance [chance 0]
           #:destination [destination (location 1 1)]
           input)
    (parameterize ([direction-change-chance chance])
      (choose (gathering direction-east destination) input))))

;If there's nothing nearby, keep moving in the same direction

(test-case:
 "move in current direction"
 (let ([choice (gather-with (choose-input))])
   (check-equal? (choice-type choice) request-move)
   (check-equal? (choice-parameter choice) (location 2 1))))
  
;Randomly change direction

(test-case:
 "move in random direction"
 (let ([choice (gather-with #:chance 1 (choose-input))])
   (check-equal? (choice-type choice) request-move)
   (check-not-equal? (choice-parameter choice) (location 2 1))))
  
;If the current movement is stopped, change direction.

(test-case:
 "change direction if can't move"
 (let ([choice (gather-with (choose-input #:success #f #:command request-move))])
   (check-equal? (choice-type choice) request-move)
   (check-not-equal? (choice-parameter choice) (location 2 1))))
  
;If a block is nearby, take it.

(test-case:
 "take nearby block"
 (let ([choice (gather-with
                (choose-input #:neighbors (list (occupant (entity 102 type-block) (location 1 0)))))])
   (check-equal? (choice-type choice) request-take)
   (check-equal? (choice-parameter choice) 102)
   (check-equal? (choice-direction choice) direction-south)))
  
;Transfer to base

(test-case:
 "transfer adjacent to base"
 (let ([choice (gather-with
                (choose-input #:neighbors (list (occupant (entity 103 type-base) (location 0 1)))
                              #:cargo (entity 103 type-block)))])
   (check-equal? (choice-type choice) request-transfer)
   (check-equal? (choice-parameter choice) 103)
   (check-equal? (choice-direction choice) direction-east)))

;The strategy makes a choice.

(define (choose spec input)
  (if (bot-cargo (action-bot input))
      (return-to-base spec input)
      (look-for-blocks spec input)))

(define (return-to-base spec input)
  (define (pick-direction)
    (direction-from
     (bot-location (action-bot input)) (gathering-destination spec)))
  (let ([bases (adjacent-entities (action-bot input) type-base)])
    (if (> (length bases) 0)
        (choose-transfer (action-bot input) (first bases))
        (choose-move (pick-direction) (gathering-direction spec) input))))

(define (look-for-blocks spec input)
  (define (pick-direction)
    (let ([old-direction (gathering-direction spec)])
      (if (> (direction-change-chance) (random))
          (change-direction (action-bot input) old-direction)
          old-direction)))
  (let ([blocks (adjacent-entities (action-bot input) type-block)])
    (if (> (length blocks) 0)
        (choose-take (action-bot input) (first blocks))
        (choose-move (pick-direction) (gathering-direction spec) input))))
