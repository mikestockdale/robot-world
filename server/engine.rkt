#lang racket

(provide make-engine engine-grid engine-board
         entity-info is-available?
         add-entity move-entity take-entity drop-entity transfer-entity)

(require "shared.rkt" "board.rkt" "grid.rkt" "sequence.rkt")
(module+ test (require rackunit "testing.rkt"))

;@title{Engine}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/engine.rkt" "engine.rkt"]}
;The engine performs the core game actions.
;It uses a sequence, and a grid.
;The sequence provides new ids for adding entities.
;The grid keeps track of all the entities.

(struct engine (sequence board grid))
(define (make-engine width height) (engine (make-sequence) (board width height) (make-grid)))

;@elemtag["available"]{A location @bold{is available} when it is @elemref["valid"]{valid} and there is no entity at that location}.

(test-case:
 "available locations"
 (test-engine
  ((size 3 2) (block 1 1))
  (check-false (is-available? engine (location 1 1)))
  (check-true (is-available? engine (location 2 1)))
  (check-false (is-available? engine (location 3 1)))))

(define (is-available? engine location)
  (and (is-valid? (engine-board engine) location)
       (not (entity-at (engine-grid engine) location))))

;The engine can @bold{add} an @bold{entity}.
;The location must be @elemref["available"]{available}.
;Each entity has a unique id.

(test-case:
 "entity is added at location"
 (let* ([engine (make-engine 9 10)]
        [block (add-entity engine type-block (location 1 2))])
   (check-equal? (entity-type block) type-block)  
   (check-equal? (occupant-by-id (engine-grid engine) (entity-id block))
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
  (if (is-available? engine location)
      (let ([new-entity (entity ((engine-sequence engine)) type)])
        (place-entity (engine-grid engine) new-entity location)
        new-entity)
      #f))

;The engine can @bold{move} an @bold{entity} in a direction.
;The destination of the move must be @elemref["available"]{available}.
;Otherwise, the entity remains in its original location.

(test-case:
 "move bot changes location"
 (test-engine
  ((size 9 10) (bot 5 6))
  (move-entity engine bot-id direction-north)
  (check-equal? (occupant-place (occupant-by-id (engine-grid engine) bot-id))
                (location 5 7))))

(test-case:
 "invalid move leaves bot location unchanged"
 (test-engine
  ((size 11 10) (bot 9 9))
  (check-false (move-entity engine bot-id direction-north))
  (check-equal? (occupant-place (occupant-by-id (engine-grid engine) bot-id))
                (location 9 9)))) 

(test-case:
 "can not move to occupied location"
 (test-engine
  ((size 4 3) (bot 1 1) (block 1 2))
  (check-false (move-entity engine bot-id direction-north))
  (check-equal? (occupant-place (occupant-by-id (engine-grid engine) bot-id))
                (location 1 1))))

;The result is not false if successful, otherwise it is @racket[#f].

(define (move-entity engine id direction)
  (let* ([old-occupant (occupant-by-id (engine-grid engine) id)]
         [new-location (move-direction direction (occupant-place old-occupant))])
    (and (is-available? engine new-location)
         (place-entity (engine-grid engine)
                       (occupant-entity old-occupant) new-location))))

;The engine can @bold{take} an @bold{entity} as cargo.

(test-case:
 "block is taken"
 (test-engine
  ((size 3 4) (bot 1 1) (block 2 1))
  (check-not-false (take-entity engine bot-id block-id))
  (check-equal? (occupant-by-id (engine-grid engine) block-id)
                (occupant block bot-id))))

;The entity being taken is loaded into the cargos table and removed from the grid.
;The result is not false if successful, otherwise it is @racket[#f].

(define (take-entity engine id cargo-id)
  (let ([cargo (occupant-by-id (engine-grid engine) cargo-id)])
    (and cargo
         (place-entity (engine-grid engine) (occupant-entity cargo) id)))) 

;The engine can @bold{drop} an @bold{entity} that is the cargo for a bot.
;The destination of the drop must be @elemref["available"]{available}.
;Otherwise, the entity remains as cargo.

(test-case:
 "block is dropped"
 (test-engine
  ((size 3 4) (block 2 1) (bot 1 1))
  (take-entity engine (entity-id bot) block-id)
  (check-not-false (drop-entity engine bot-id direction-north))
  (check-equal? (occupant-place (occupant-by-id (engine-grid engine) block-id)) (location 1 2))))

(test-case:
 "can not drop in occupied location"
 (test-engine
  ((size 3 4) (block1 2 1) (bot 1 1) (block2 0 1))
  (take-entity engine bot-id block1-id)
  (check-false (drop-entity engine bot-id direction-west))
  (check-equal? (occupant-place (occupant-by-id (engine-grid engine) block1-id)) bot-id)))

;The entity being dropped is unloaded from the cargos table and placed in the grid.
;The result is not false if successful, otherwise it is @racket[#f].

(define (drop-entity engine id direction)
  (let* ([bot (occupant-by-id (engine-grid engine) id)]
         [drop-location (move-direction direction (occupant-place bot))])
    (and (is-available? engine drop-location)
         (place-entity (engine-grid engine)
                       (entity-at (engine-grid engine) id)
                       drop-location))))

;The engine can @bold{transfer} an @bold{entity} from a bot to a base.
;The bot must be adjacent to the base.

(test-case:
 "transfer"
 (test-engine
  ((size 3 4) (bot 1 1) (base 1 2) (block 2 1))
  (take-entity engine bot-id block-id)
  (transfer-entity engine bot-id base-id)
  (check-equal? (occupant-place (occupant-by-id (engine-grid engine) block-id)) base-id)))

(test-case:
 "must be adjacent"
 (test-engine
  ((size 3 4) (bot 1 1) (base 2 2) (block 2 1))
  (take-entity engine bot-id block-id)
  (check-false (transfer-entity engine bot-id base-id))))

;The entity being transferred is removed from the bot's cargo and added to the base's cargo.
;The result is not false if successful, otherwise it is @racket[#f].

(define (transfer-entity engine from-id to-id)
  (let ([from-entity (occupant-by-id (engine-grid engine) from-id)]
        [to-entity (occupant-by-id (engine-grid engine) to-id)])
    (and
     (adjacent? (occupant-place from-entity) (occupant-place to-entity))
     (place-entity (engine-grid engine)
                   (entity-at (engine-grid engine) from-id)
                   to-id))))

;@bold{Neighbors} of a location are all the nearby entites, plus any edges.

(test-case:
 "neighbors are nearby"
 (test-engine
  ((size 4 3) (block1 2 2) (block2 3 1))
  (let ([neighbors (neighbors engine (location 1 1))])
    (check-equal? (length neighbors) 1)
    (check-equal? (occupant-place (first neighbors)) (location 2 2)))))

(test-case:
 "neighbors include edges"
 (test-engine
  ((size 3 4))
  (let ([neighbors (neighbors engine (location 0 1))])
    (check-equal? (length neighbors) 1)
    (check-equal? (entity-type (occupant-entity (first neighbors))) type-edge)
    (check-equal? (occupant-place (first neighbors)) (location -1 1)))))

(define (neighbors engine location)
  (append
   (edges (engine-board engine)location)
   (occupants-nearby (engine-grid engine) location)))

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
 
;The bot and its neighbors are retrieved from the grid and the cargo from the cargos table.

(define (entity-info engine entity-id)
  (let ([occupant (occupant-by-id (engine-grid engine) entity-id)])
    (values
     occupant
     (entity-at (engine-grid engine) entity-id)
     (neighbors engine (occupant-place occupant)))))
