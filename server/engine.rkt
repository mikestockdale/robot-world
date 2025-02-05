#lang racket/base

(provide make-engine engine-places engine-board
         entity-info available?
         add-entity move-entity take-entity drop-entity transfer-entity)

(require racket/list)
(require "shared.rkt" "board.rkt" "places.rkt" "sequence.rkt")
(module+ test (require rackunit "testing.rkt"))

;@title{Engine}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/engine.rkt" "engine.rkt"]}
;The engine performs the core game actions.
;It uses a sequence, a board, and a table of places.
;The sequence provides new ids for adding entities.
;The board manages the size of the game area.
;The places table keeps track of all the entities.

(struct engine (sequence board places))
(define (make-engine width height)
  (engine (make-sequence) (board width height) (make-places)))

;@elemtag["available"]{A location is @bold{available} when it is @elemref["valid"]{valid} and there is no entity at that location}.
;@margin-note{@racket[test-engine] is a macro to set up test data in an engine: source at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/testing.rkt" "testing.rkt"]}

(test-case:
 "available locations"
 (test-engine
  ((size 3 2) (block 1 1))
  (check-false (available? engine (location 1 1)))
  (check-true (available? engine (location 2 1)))
  (check-false (available? engine (location 3 1)))))

(define (available? engine location)
  (and (is-valid? (engine-board engine) location)
       (not (entity-at (engine-places engine) location))))

;The engine can @bold{add} an @bold{entity}.
;The location must be @elemref["available"]{available}.
;Each entity has a unique id.

(test-case:
 "entity is added at location"
 (let* ([engine (make-engine 9 10)]
        [block (add-entity engine type-block (location 1 2))])
   (check-equal? (entity-type block) type-block)  
   (check-equal? (occupant-by-id (engine-places engine) (entity-id block))
                 (occupant block (location 1 2)))))  

(test-case:
 "entity is not added at invalid location"
 (check-false (add-entity (make-engine 10 9) type-bot (location -1 2))))

(test-case:
 "entity is created with new id"
 (let ([engine (make-engine 9 10)])
   (check-not-equal? (entity-id (add-entity engine type-bot (location 3 4)))
                     (entity-id (add-entity engine type-bot (location 5 6))))))

;The new entity is returned, if successful.
;Otherwise @racket[#f] is returned.

(define (add-entity engine type location)
  (and (available? engine location)
       (let ([new-entity (entity ((engine-sequence engine)) type)])
         (place-entity (engine-places engine) new-entity location)
         new-entity)))

;The engine can @bold{move} an @bold{entity} to a new location.
;The destination of the move must be @elemref["adjacent"]{adjacent} and @elemref["available"]{available}.
;Otherwise, the entity remains in its original location.

(test-case:
 "move bot changes location"
 (test-engine
  ((size 9 10) (bot 5 6))
  (check-not-false (move-entity engine bot-id (location 5 7)))
  (check-equal? (place-by-id (engine-places engine) bot-id)
                (location 5 7))))

(test-case:
 "invalid move leaves bot location unchanged"
 (test-engine
  ((size 11 10) (bot 9 9) (block 8 9))
  (check-false (move-entity engine bot-id (location 9 10)) "invalid location")
  (check-false (move-entity engine bot-id (location 8 9)) "not available")
  (check-false (move-entity engine bot-id (location 8 8)) "not adjacent")
  (check-equal? (place-by-id (engine-places engine) bot-id)
                (location 9 9)))) 

;The result is not false if successful, otherwise it is @racket[#f].

(define (move-entity engine id new-location)
  (let ([bot (occupant-by-id (engine-places engine) id)])
    (and (adjacent? new-location (occupant-location bot))
         (available? engine new-location)
         (place-entity (engine-places engine)
                       (occupant-entity bot) new-location))))

;The engine can @bold{take} an @bold{entity} as cargo.

(test-case:
 "block is taken"
 (test-engine
  ((size 3 4) (bot 1 1) (block 2 1))
  (check-not-false (take-entity engine bot-id block-id))
  (check-equal? (place-by-id (engine-places engine) block-id)
                bot-id)))

;The entity being taken is placed in the bot, as cargo.
;The result is not false if successful, otherwise it is @racket[#f].

(define (take-entity engine id cargo-id)
  (let ([cargo (occupant-by-id (engine-places engine) cargo-id)])
    (and cargo
         (place-entity (engine-places engine) (occupant-entity cargo) id)))) 

;The engine can @bold{drop} an @bold{entity} that is the cargo for a bot.
;The destination of the drop must be @elemref["adjacent"]{adjacent} and @elemref["available"]{available}.
;Otherwise, the entity remains as cargo.

(test-case:
 "block is dropped"
 (test-engine
  ((size 3 4) (block 2 1) (bot 1 1))
  (take-entity engine (entity-id bot) block-id)
  (check-not-false (drop-entity engine bot-id (location 1 2)))
  (check-equal? (place-by-id (engine-places engine) block-id) (location 1 2))))

(test-case:
 "can not drop in location"
 (test-engine
  ((size 3 4) (block1 2 1) (bot 1 1) (block2 0 1))
  (take-entity engine bot-id block1-id)
  (check-false (drop-entity engine bot-id (location 3 1)) "invalid")
  (check-false (drop-entity engine bot-id (location 0 1)) "not available")
  (check-false (drop-entity engine bot-id (location 2 2)) "not adjacent")
  (check-equal? (entity-at (engine-places engine) bot-id) block1)))

;The entity being dropped is placed at a location.
;The result is not false if successful, otherwise it is @racket[#f].

(define (drop-entity engine id drop-location)
  (let ([bot (occupant-by-id (engine-places engine) id)])
    (and (adjacent? drop-location (occupant-location bot))
         (available? engine drop-location)
         (place-entity (engine-places engine)
                       (entity-at (engine-places engine) id)
                       drop-location))))

;The engine can @bold{transfer} an @bold{entity} from a bot to a base.
;The bot must be adjacent to the base.

(test-case:
 "transfer"
 (test-engine
  ((size 3 4) (bot 1 1) (base 1 2) (block 2 1))
  (take-entity engine bot-id block-id)
  (transfer-entity engine bot-id base-id)
  (check-equal? (place-by-id (engine-places engine) block-id) base-id)))

(test-case:
 "must be adjacent"
 (test-engine
  ((size 3 4) (bot 1 1) (base 2 2) (block 2 1))
  (take-entity engine bot-id block-id)
  (check-false (transfer-entity engine bot-id base-id))))

;The entity being transferred is removed from the bot's cargo and added to the base's cargo.
;The result is not false if successful, otherwise it is @racket[#f].

(define (transfer-entity engine from-id to-id)
  (let ([from-entity (occupant-by-id (engine-places engine) from-id)]
        [to-entity (occupant-by-id (engine-places engine) to-id)])
    (and
     (adjacent? (occupant-location from-entity) (occupant-location to-entity))
     (place-entity (engine-places engine)
                   (entity-at (engine-places engine) from-id)
                   to-id))))

;@bold{Neighbors} of a location are all the nearby entities, plus any edges.

(test-case:
 "neighbors are nearby"
 (test-engine
  ((size 4 3) (block1 2 2) (block2 3 1))
  (let ([neighbors (neighbors engine (location 1 1))])
    (check-equal? (length neighbors) 1)
    (check-equal? (occupant-location (first neighbors)) (location 2 2)))))

(test-case:
 "neighbors include edges"
 (test-engine
  ((size 3 4))
  (let ([neighbors (neighbors engine (location 0 1))])
    (check-equal? (length neighbors) 1)
    (check-equal? (occupant-type (first neighbors)) type-edge)
    (check-equal? (occupant-location (first neighbors)) (location -1 1)))))

(define (neighbors engine bot-location)
  (define (nearby entity location)
    (and (nearby? location bot-location)
         (occupant entity location)))
  (append
   (edges (engine-board engine) bot-location)
   (filter-map-occupants (engine-places engine) nearby)))

;The engine provides @bold{entity info} to be returned to the client.

(test-case:
 "entity info"
 (test-engine
  ((size 5 4) (bot1 2 2) (block1 2 3) (block2 1 1))
  (take-entity engine bot1-id block1-id)
  (let-values ([(bot-occupant cargo neighbors) (entity-info engine bot1-id)])
    (check-equal? bot-occupant (occupant bot1 (location 2 2)))
    (check-equal? cargo block1)
    (check-equal? neighbors (list (occupant block2 (location 1 1)))))))
 
;The bot, its cargo, and its neighbors are retrieved from the places table.

(define (entity-info engine entity-id)
  (let ([occupant (occupant-by-id (engine-places engine) entity-id)])
    (values
     occupant
     (entity-at (engine-places engine) entity-id)
     (neighbors engine (occupant-location occupant)))))
