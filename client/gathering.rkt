#lang racket

(provide gathering-actions)

(require "shared.rkt" "action.rkt" "bot.rkt" "tactics.rkt")
(module+ test (require rackunit))

;@title{Gathering}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/gathering.rkt" "gathering.rkt"]}
;Gathering is a modification of the wandering strategy.
;The bots still wander randomly, taking blocks when they find them, but they bring them to a fixed location to drop them.
;This results in blocks being grouped more rapidly into a single cluster.
;Its purpose is as an second example, to help evolve the structure of the game.

;The strategy keeps track of the current direction each bot is facing.
;To avoid repeatedly taking and dropping the same block, it also maintains a delay in between these actions.
;It has a destination location, where the blocks are gathered.

(struct gathering (direction cargo-delay destination))

;At the start of the game, a list of actions is generated from the list of bots assigned to the client.

(define (gathering-actions replies)
  (map (λ (reply)
         (action (gather (gathering direction-east 0 (location 25 25)))
                 #f #f #t (make-bot reply)))
       replies))

;At each turn, a choice is made for each bot and the action is updated. 

(define ((gather spec) input-action)
  (let ([choice (choose spec input-action)])
    (struct-copy
     action input-action
     [request-type (choice-type choice)]
     [parameter (choice-parameter choice)]
     [strategy (gather (struct-copy
                        gathering spec
                        [direction (choice-direction choice)]
                        [cargo-delay (choice-delay choice)]))])))

;A couple of helper methods for testing

(module+ test
  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action #f command #f success
            (bot (entity 101 type-bot (location 1 1)) cargo neighbors))))

(module+ test
  (define (gather-with
           #:chance [chance 0]
           #:cargo-delay [cargo-delay 0]
           input)
    (parameterize ([direction-change-chance chance])
      (choose (gathering direction-east cargo-delay (location 1 1)) input))))

;If there's nothing nearby, keep moving in the same direction

(test-case:
 "move in current direction"
 (let ([choice (gather-with #:cargo-delay 5 (choose-input))])
   (check-equal? (choice-type choice) request-move)
   (check-equal? (choice-parameter choice) direction-east)
   (check-equal? (choice-delay choice) 4)))
  
;Randomly change direction

(test-case:
 "move in random direction"
 (let ([choice (gather-with #:chance 1 (choose-input))])
   (check-equal? (choice-type choice) request-move)
   (check-not-equal? (choice-parameter choice) direction-east)))
  
;If the current movement is stopped, change direction.

(test-case:
 "change direction if can't move"
 (let ([choice (gather-with (choose-input #:success #f #:command request-move))])
   (check-equal? (choice-type choice) request-move)
   (check-not-equal? (choice-parameter choice) direction-east)))
  
;If a block is nearby, take it, and start a delay.

(test-case:
 "take nearby block"
 (let ([choice (gather-with
                (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
   (check-equal? (choice-type choice) request-take)
   (check-equal? (choice-parameter choice) 102)
   (check-equal? (choice-direction choice) direction-south)
   (check-equal? (choice-delay choice) 5)))
  
;If the delay hasn't elapsed yet, don't take the block.

(test-case:
 "delay taking nearby block"
 (let ([choice (gather-with #:cargo-delay 1
                            (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
   (check-equal? (choice-type choice) request-move)))

;If a block is nearby, drop the cargo block, and start a delay.

(test-case:
 "drop nearby block"
 (let ([choice (gather-with
                (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                              #:cargo (entity 103 type-block (location 0 0))))])
   (check-equal? (choice-type choice) request-drop)
   (check-equal? (choice-parameter choice) direction-north)
   (check-equal? (choice-delay choice) 5)
   (check-not-equal? (choice-direction choice) direction-north)))

;If the delay hasn't elapsed yet, don't drop the block.

(test-case:
 "delay dropping nearby block"
 (let ([choice (gather-with #:cargo-delay 1
                            (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                          #:cargo (entity 103 type-block (location 0 0))))])
   (check-equal? (choice-type choice) request-move)))

;Drop at target location

(test-case:
 "drop at destination"
 (let ([choice (gather-with (choose-input #:cargo (entity 103 type-block (location 0 0))))])
   (check-equal? (choice-type choice) request-drop)))

;The strategy makes a choice.

(define (choose spec input)
  (define (pick-direction)
    (if (bot-cargo (action-bot input))
        (direction-from
         (bot-location (action-bot input)) (gathering-destination spec))
        (let ([old-direction (gathering-direction spec)])
          (if (or (and (equal? (action-request-type input) request-move)
                       (not (action-success? input))) 
                  (> (direction-change-chance) (random)))
              (change-direction old-direction)
              old-direction))))
  (if (and (= (gathering-cargo-delay spec) 0)
           (bot-cargo (action-bot input))
           (or
            (blocks-nearby? (action-bot input))
            (equal? (gathering-destination spec) (bot-location (action-bot input)))))
      (choose-drop (action-bot input))
      (let ([blocks (removable-blocks (action-bot input))])
        (if (and (= (gathering-cargo-delay spec) 0)
                 (> (length blocks) 0))
            (choose-take (action-bot input) (first blocks))
            (choose-move (pick-direction) (gathering-cargo-delay spec))))))
