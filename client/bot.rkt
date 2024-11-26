#lang racket

(provide (struct-out bot) make-bot bot-id bot-location
         is-free? change-direction)

(require "shared.rkt")
(module+ test (require rackunit))

;@title{Bot}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/bot.rkt" "bot.rkt"]}
;A bot is an entity that is controlled by client code.
;It can pick up a block entity.
;This is stored in the @racket[cargo] field.
;It also has a list of @elemref["nearby"]{nearby} entities that it can see, in the @racket[neighbors] field.

(struct bot (entity cargo neighbors))
(define (make-bot reply) (bot (reply-entity reply) (reply-cargo reply) (reply-neighbors reply)))

;We include a couple of helper functions, to access the entity id and location data.

(define (bot-id bot) (entity-id (bot-entity bot)))
(define (bot-location bot) (entity-location (bot-entity bot)))

;A location @bold{is free} if there are no entities at the location.

(test-case:
 "free location found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block (entity 102 type-block (location 1 2))]
        [bot (bot bot1 #f (list block))])
   (check-true (is-free? bot (location 2 1)))
   (check-false (is-free? bot (location 1 2)))))

;We check the locations of bot's neighbors.

(define (is-free? bot location)
  (not (findf (λ (neighbor) (equal? location (entity-location neighbor)))
              (bot-neighbors bot))))

;A bot can @bold{change direction}.

(test-case:
 "new direction is different"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block (entity 102 type-block (location 1 2))]
        [bot (bot bot1 #f (list block))]
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
