#lang racket

(provide wandering-actions)

(require "action.rkt" "bot.rkt" "shared.rkt" "tactics.rkt")
(module+ test (require rackunit))

;@title{Wandering}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/wandering.rkt" "wandering.rkt"]}
;Wandering is a strategy where bots wander randomly, taking blocks when they find them, and dropping them nearby other blocks.
;This results in blocks being grouped into clusters.

;Of course, this isn't really a strategy, and at this point the game does not have any goals that a strategy can pursue anyway.
;It is a partial copy of the current strategy in the orginal version of the game.
;Its main purpose is as an example, to help evolve the structure of the game.

;The strategy keeps track of the current direction each bot is facing.
;To avoid repeatedly taking and dropping the same block, it also maintains a delay in between these actions.

(struct wandering (direction cargo-delay))

;At the start of the game, a list of actions is generated from the list of bots assigned to the client.

(define (wandering-actions replies)
  (map
   (λ (reply) (action (wander (wandering direction-east 0)) #f #f #t (make-bot reply)))
   replies))

;At each turn, a choice is made for each bot and the action is updated. 

(define ((wander spec) input-action)
  (let ([choice (choose spec input-action)])
    (struct-copy
     action input-action
     [request-type (choice-type choice)]
     [parameter (choice-parameter choice)]
     [strategy (wander (wandering spec (choice-direction choice) (choice-delay choice)))])))

;A couple of helper methods for testing

(module+ test
  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action choose command #f success
            (bot (entity 101 type-bot (location 1 1)) cargo neighbors))))
  
(module+ test
  (define (wander-with
           #:chance [chance 0]
           #:cargo-delay [cargo-delay 0]
           input)
    (parameterize ([direction-change-chance chance])
      (choose (wandering direction-east cargo-delay) input))))

;If there's nothing nearby, keep moving in the same direction

(test-case:
 "move in current direction"
 (let ([choice (wander-with #:cargo-delay 5 (choose-input))])
   (check-equal? (choice-type choice) request-move)
   (check-equal? (choice-parameter choice) direction-east)
   (check-equal? (choice-delay choice) 4)))

;Randomly change direction
  
(test-case:
 "move in random direction"
 (let ([choice (wander-with #:chance 1 (choose-input))])
   (check-equal? (choice-type choice) request-move)
   (check-not-equal? (choice-parameter choice) direction-east)))

;If the current movement is stopped, change direction.

(test-case:
 "change direction if can't move"
 (let ([choice (wander-with (choose-input #:success #f #:command request-move))])
   (check-equal? (choice-type choice) request-move)
   (check-not-equal? (choice-parameter choice) direction-east)))

;If a block is nearby, take it, and start a delay.

(test-case:
 "take nearby block"
 (let ([choice (wander-with
                (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
   (check-equal? (choice-type choice) request-take)
   (check-equal? (choice-parameter choice) 102)
   (check-equal? (choice-direction choice) direction-south)
   (check-equal? (choice-delay choice) 5)))

;If the delay hasn't elapsed yet, don't take the block.

(test-case:
 "delay taking nearby block"
 (let ([choice (wander-with
                #:cargo-delay 1
                (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
   (check-equal? (choice-type choice) request-move)))

;If a block is nearby, drop the cargo block, and start a delay.

(test-case:
 "drop nearby block"
 (let ([choice (wander-with
                (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                              #:cargo (entity 103 type-block (location 0 0))))])
   (check-equal? (choice-type choice) request-drop)
   (check-equal? (choice-parameter choice) direction-north)
   (check-equal? (choice-delay choice) 5)
   (check-not-equal? (choice-direction choice) direction-north)))

;If the delay hasn't elapsed yet, don't drop the block.

(test-case:
 "delay dropping nearby block"
 (let ([choice (wander-with
                #:cargo-delay 1
                (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                              #:cargo (entity 103 type-block (location 0 0))))])
   (check-equal? (choice-type choice) request-move)))

;The strategy makes a choice.

(define (choose spec input)
  (define (pick-direction)
    (let ([old-direction (wandering-direction spec)])
      (if (or (and (equal? (action-request-type input) request-move)
                   (not (action-success? input))) 
              (> (direction-change-chance) (random)))
          (change-direction old-direction)
          old-direction)))
  (if (and (= (wandering-cargo-delay spec) 0)
           (bot-cargo (action-bot input))
           (blocks-nearby? (action-bot input)))
      (choose-drop (action-bot input))
      (let ([blocks (removable-blocks (action-bot input))])
        (if (and (= (wandering-cargo-delay spec) 0)
                 (> (length blocks) 0))
            (choose-take (action-bot input) (first blocks))
            (choose-move (pick-direction) (wandering-cargo-delay spec))))))
