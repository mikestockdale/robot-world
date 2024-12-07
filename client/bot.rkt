#lang racket

(provide (struct-out bot) make-bot bot-id
         is-free? adjacent-blocks change-direction)

(require "shared.rkt")
(module+ test (require rackunit))

;@title{Bot}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/bot.rkt" "bot.rkt"]}
;A bot is an entity that is controlled by client code.
;It can pick up a block entity.
;This is stored in the @racket[cargo] field.
;It also has a list of @elemref["nearby"]{nearby} entities that it can see, in the @racket[neighbors] field.

(struct bot (entity location cargo neighbors))

(define (make-bot reply)
  (bot (reply-entity reply) (reply-location reply) (reply-cargo reply)
       (reply-neighbors reply)))

;We include a couple of helper functions, to access the entity id and location data.

(define (bot-id bot) (entity-id (bot-entity bot)))

;A location @bold{is free} if there are no entities at the location.

(test-case:
 "free location checked"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block (entity 102 type-block #f)]
        [bot (bot bot1 (location 1 1) #f (list (occupant block (location 1 2))))])
   (check-true (is-free? bot (location 2 1)))
   (check-false (is-free? bot (location 1 2)))))

;We check the locations of bot's neighbors.

(define (is-free? bot location)
  (not (findf (λ (neighbor) (equal? location (occupant-place neighbor)))
              (bot-neighbors bot))))

;@bold{Adjacent blocks} are ones that are adjacent to a bot.

(test-case:
 "adjacent block found"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block1 (entity 102 type-block #f)]
        [block2 (entity 102 type-block #f)]
        [bot2 (entity 103 type-bot #f)]
        [bot (bot bot1 (location 1 1) #f
                  (list (occupant bot2 (location 2 1))
                        (occupant block1 (location 1 2))
                        (occupant block2 (location 2 2))))]
        [adjacent (adjacent-blocks bot)])
   (check-equal? (length adjacent) 1)
   (check-equal? (first adjacent) (occupant block1 (location 1 2)))))
  
(test-case:
 "block not adjacent"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block1 (entity 102 type-block #f)]
        [block2 (entity 102 type-block #f)]
        [bot (bot bot1 (location 1 1) #f
                  (list (occupant block1 (location 0 2))
                        (occupant block2 (location 2 2))))]
        [adjacent (adjacent-blocks bot)])
   (check-equal? (length adjacent) 0)))

;We filter the bot's neighbors to find adjacent blocks

(define (adjacent-blocks bot)
  (filter (λ (neighbor)
            (and (= (entity-type (occupant-entity neighbor)) type-block)
                 (adjacent? (occupant-place neighbor) (bot-location bot))))
          (bot-neighbors bot)))

;A bot can @bold{change direction}.

(test-case:
 "new direction is different"
 (let* ([bot1 (entity 101 type-bot #f)]
        [block (entity 102 type-block #f)]
        [bot (bot bot1 (location 1 1) #f (list (occupant block (location 1 2))))]
        [new (change-direction bot direction-west)])
   (check-true (or (= new direction-east) (= new direction-south)))
   (check-false (and (= new direction-north) (= new direction-west)))))

;A new direction is chosen at random from the valid candidates.
;If there are no candidates, the direction is unchanged.

(define (change-direction bot old-direction)
  (define (valid-change? new-location) (is-free? bot new-location))
  (let* ([candidates (filter valid-change? (all-directions (bot-location bot) #:except old-direction))]
         [count (length candidates)])
    (if (= count 0)
        old-direction
        (direction-from (bot-location bot) (list-ref candidates (random count))))))
