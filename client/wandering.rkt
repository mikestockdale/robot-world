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

;There are @bold{blocks nearby} if they are in the bot's neighbor list.

(test-case:
 "blocks nearby"
 (check-true (blocks-nearby? (bot #f #f #f (list (occupant (entity 101 type-block #f) #f)))))
 (check-false (blocks-nearby? (bot #f #f #f (list (occupant (entity 101 type-bot #f) #f)))))
 (check-false (blocks-nearby? (bot #f #f #f '()))))

(define (blocks-nearby? bot)
  (ormap (λ (neighbor) (= (entity-type (occupant-entity neighbor)) type-block))
         (bot-neighbors bot)))

;When a strategy @bold{choose}s to @bold{drop} a block, the choice parameter is the direction to a free location.
;The next move direction is different from the drop direction, and the delay before taking another block is 5 turns.

(test-case:
 "free location found"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block (entity 102 type-block (location 1 2))]
        [bot (bot bot1 (location 1 1) #f (list (occupant block (location 1 2))))]
        [choice (choose-drop bot)])
   (check-equal? (choice-type choice) request-drop)
   (check-equal? (choice-parameter choice) direction-east)
   (check-not-equal? (choice-direction choice) direction-east)
   (check-equal? (choice-delay choice) 5)))

;The direction chosen places the block in a location with the most adjacent blocks.

(test-case:
 "best location found"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block1 (entity 102 type-block (location 0 0))]
        [block2 (entity 103 type-block (location 2 0))]
        [choice (choose-drop (bot bot1 (location 1 1) #f
                                  (list (occupant block1 (location 0 0))
                                        (occupant block2 (location 2 0)))))])
   (check-equal? (choice-parameter choice) direction-south)))

;The direction chosen doesn't move outside the world
    
(test-case:
 "free location not outside world"
 (let* ([bot1 (entity 101 type-bot #f)]
        [edge1 (make-edge (location 50 49))]
        [edge2 (make-edge (location 49 50))]
        [block (entity 102 type-block (location 49 48))]
        [choice (choose-drop (bot bot1 (location 49 49) #f
                                  (list (occupant edge1 (location 50 49))
                                        (occupant edge2  (location 49 50))
                                        (occupant block (location 49 48)))))])
   (check-equal? (choice-parameter choice) direction-west)))

;The chosen direction is the one to the free location with most adjacent blocks

(define (choose-drop bot)
  (define (score location)
    (if (is-free? bot location) (count-adjacent bot location) -1))
  (let* ([drop-location (argmax score (all-directions (bot-location bot)))]
         [drop-direction (direction-from (bot-location bot) drop-location)])
    (choice request-drop drop-direction
            (change-direction bot drop-direction) 5)))

(define (count-adjacent bot location)
  (define (adjacent-block? neighbor)
    (and (= (entity-type (occupant-entity neighbor)) type-block)
         (adjacent? (occupant-place neighbor) location)))
  (count adjacent-block? (bot-neighbors bot)))

;@bold{Removeable blocks} are ones that are adjacent to a bot and are not adjacent to two other blocks.

(test-case:
 "removable block found"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block1 (entity 102 type-block (location 1 2))]
        [block2 (entity 102 type-block (location 2 2))]
        [bot2 (entity 103 type-bot (location 2 1))]
        [bot (bot bot1 (location 1 1) #f
                  (list (occupant bot2 (location 2 1))
                        (occupant block1 (location 1 2))
                        (occupant block2 (location 2 2))))]
        [removable (removable-blocks bot)])
   (check-equal? (length removable) 1)
   (check-equal? (occupant-entity (first removable)) block1)))
  
(test-case:
 "block not removable"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block1 (entity 102 type-block (location 1 2))]
        [block2 (entity 102 type-block (location 2 2))]
        [block3 (entity 103 type-block (location 0 2))]
        [bot (bot bot1 (location 1 1) #f
                  (list (occupant block1 (location 1 2))
                                (occupant block2 (location 2 2))
                                (occupant block3 (location 0 2))))]
        [removable (removable-blocks bot)])
   (check-equal? (length removable) 0)))

;We filter the bot's neighbors to find removable blocks

(define (removable-blocks bot)
  (filter (λ (neighbor)
            (and (= (entity-type (occupant-entity neighbor)) type-block)
                 (adjacent? (occupant-place neighbor) (bot-location bot))
                 (< (count-adjacent bot (occupant-place neighbor)) 2)))
          (bot-neighbors bot)))

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
     [strategy (wander (wandering (choice-direction choice) (choice-delay choice)))])))

;A couple of helper methods for testing

(module+ test
  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action choose command #f success
            (bot (entity 101 type-bot #f) (location 1 1) cargo neighbors))))
  
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
                (choose-input #:neighbors (list (occupant (entity 102 type-block #f) (location 1 0)))))])
   (check-equal? (choice-type choice) request-take)
   (check-equal? (choice-parameter choice) 102)
   (check-equal? (choice-direction choice) direction-south)
   (check-equal? (choice-delay choice) 5)))

;If the delay hasn't elapsed yet, don't take the block.

(test-case:
 "delay taking nearby block"
 (let ([choice (wander-with
                #:cargo-delay 1
                (choose-input #:neighbors (list (occupant (entity 102 type-block #f) (location 1 0)))))])
   (check-equal? (choice-type choice) request-move)))

;If a block is nearby, drop the cargo block, and start a delay.

(test-case:
 "drop nearby block"
 (let ([choice (wander-with
                (choose-input #:neighbors (list (occupant (entity 102 type-block #f) (location 2 2)))
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
                (choose-input #:neighbors (list (occupant (entity 102 type-block #f) (location 2 2)))
                              #:cargo (entity 103 type-block (location 0 0))))])
   (check-equal? (choice-type choice) request-move)))

;The strategy makes a choice.

(define (choose spec input)
  (define (pick-direction)
    (let ([old-direction (wandering-direction spec)])
      (if (or (and (equal? (action-request-type input) request-move)
                   (not (action-success? input))) 
              (> (direction-change-chance) (random)))
          (change-direction (action-bot input) old-direction)
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
