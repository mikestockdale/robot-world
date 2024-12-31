#lang racket/base

(provide (struct-out bot) make-bot bot-id
         adjacent-entities change-direction)

(require racket/list)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Bot}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/bot.rkt" "bot.rkt"]}
;A bot is an entity that is controlled by client code.
;It can pick up a block entity.
;This is stored in the @racket[cargo] field.
;It also has a list of @elemref["nearby"]{nearby} entities that it can see, in the @racket[neighbors] field.

(struct bot (id location cargo neighbors))

(define (make-bot reply)
  (bot (entity-id (reply-entity reply)) (reply-location reply) (reply-cargo reply)
       (reply-neighbors reply)))

;@bold{Adjacent entities} are ones at locations adjacent to the bot's location.

(test-case:
 "adjacent block found"
 (let* ([bot1 (entity 101 type-bot)]
        [block1 (entity 102 type-block)]
        [block2 (entity 102 type-block)]
        [bot2 (entity 103 type-bot)]
        [bot (bot bot1 (location 1 1) #f
                  (list (occupant bot2 (location 2 1))
                        (occupant block1 (location 1 2))
                        (occupant block2 (location 2 2))))]
        [adjacent (adjacent-entities bot type-block)])
   (check-equal? (length adjacent) 1)
   (check-equal? (first adjacent) (occupant block1 (location 1 2)))))
  
(test-case:
 "block not adjacent"
 (let* ([bot1 (entity 101 type-bot)]
        [block1 (entity 102 type-block)]
        [block2 (entity 102 type-block)]
        [bot (bot bot1 (location 1 1) #f
                  (list (occupant block1 (location 0 2))
                        (occupant block2 (location 2 2))))]
        [adjacent (adjacent-entities bot type-block)])
   (check-equal? (length adjacent) 0)))

;We filter the bot's neighbors to find adjacent entities of a requested type.

(define (adjacent-entities bot type)
  (filter (λ (neighbor)
            (and (= (occupant-type neighbor) type)
                 (adjacent? (occupant-location neighbor) (bot-location bot))))
          (bot-neighbors bot)))

;A bot can @bold{change direction}.

(test-case:
 "new direction is different"
 (let* ([bot1 (entity 101 type-bot)]
        [block (entity 102 type-block)]
        [bot (bot bot1 (location 1 1) #f (list (occupant block (location 1 2))))]
        [new (change-direction bot direction-west)])
   (check-true (or (equal? new direction-east) (equal? new direction-south)))
   (check-false (and (equal? new direction-north) (equal? new direction-west)))))

;A new direction is chosen at random from the valid candidates.
;If there are no candidates, the direction is unchanged.

(define (change-direction bot old-direction)
  (define (valid-change? location)
    (not (findf (λ (neighbor) (equal? location (occupant-location neighbor)))
                (bot-neighbors bot))))
  (let* ([candidates
          (filter valid-change? (all-directions (bot-location bot) #:except old-direction))]
         [count (length candidates)])
    (if (= count 0)
        old-direction
        (direction-from (bot-location bot) (list-ref candidates (random count))))))
