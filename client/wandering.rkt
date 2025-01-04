#lang racket/base

(provide wandering-steps)

(require racket/list)
(require "shared.rkt" "step.rkt" "bot.rkt" "tactics.rkt")
(module+ test (require rackunit))

;@title{Wandering}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/wandering.rkt" "wandering.rkt"]}
;The @bold{wandering} strategy is a very simple way to find bots.
;The bots start adjacent to a base.
;The bots wander randomly, taking blocks when they find them, and then bringing them back to their starting location to transfer them to the base.

;The strategy keeps track of the current direction each bot is facing.
;It has an origin location, where the blocks are returned to be transfered.

(struct strategy (direction origin))

;At the start of the game, a list of steps is generated from the list of bots assigned to the client.

(define (wandering-steps replies)
  (map (λ (reply)
         (let ([bot (make-bot reply)])
           (step (wander (strategy direction-east (bot-location bot)))
                   #f #f #t bot)))
       replies))

;At each turn, a choice is made for each bot.
;Two values are returned: the strategy function for the next turn, and the server request for this turn. 

(define ((wander spec) step)
  (let ([choice (choose spec step)])
    (values
     (wander (struct-copy
              strategy spec
              [direction (choice-direction choice)]))
     (request (choice-type choice)
              (step-id step)
              (choice-parameter choice)))))

;A couple of helper methods for testing

(module+ test
  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (step #f command #f success
            (bot (entity 101 type-bot) (location 1 1) cargo neighbors))))

(module+ test
  (define (gather-with
           #:chance [chance 0]
           #:destination [destination (location 1 1)]
           input)
    (parameterize ([direction-change-chance chance])
      (choose (strategy direction-east destination) input))))

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
                              #:cargo (entity 102 type-block)))])
   (check-equal? (choice-type choice) request-transfer)
   (check-equal? (choice-parameter choice) 103)
   (check-equal? (choice-direction choice) direction-east)))

;The strategy makes a choice.

(define (choose strategy step)
  (if (bot-cargo (step-bot step))
      (return-to-base strategy step)
      (look-for-blocks strategy step)))

(define (return-to-base strategy step)
  (define (return-direction)
    (direction-from
     (step-location step) (strategy-origin strategy)))
  (let ([bases (adjacent-entities (step-bot step) type-base)])
    (if (> (length bases) 0)
        (choose-transfer (step-bot step) (first bases))
        (choose-move (return-direction) (strategy-direction strategy) step))))

(define (look-for-blocks strategy step)
  (define (pick-direction)
    (let ([old-direction (strategy-direction strategy)])
      (if (> (direction-change-chance) (random))
          (change-direction (step-bot step) old-direction)
          old-direction)))
  (let ([blocks (adjacent-entities (step-bot step) type-block)])
    (if (> (length blocks) 0)
        (choose-take (step-bot step) (first blocks))
        (choose-move (pick-direction) (strategy-direction strategy) step))))
