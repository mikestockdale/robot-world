#lang racket

(provide make-engine entity-info draw-entities add-base-at-random
         add-entity move-entity take-entity drop-entity transfer-entity)

(require threading)
(require "shared.rkt" "cargos.rkt" "grid.rkt" "sequence.rkt")
(module+ test (require rackunit "testing.rkt"))

;@title{Engine}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/engine.rkt" "engine.rkt"]}
;The engine performs the core game actions.
;It uses a sequence, a grid and a cargos table.
;The sequence provides new ids for adding entities.
;The grid keeps track of all the entities.
;The cargos are the blocks being carried by bots.

(struct engine (sequence grid cargos))
(define (make-engine size) (engine (make-sequence) (make-grid size) (make-cargos)))

;The engine can @bold{add} an @bold{entity}.
;The location must be @elemref["available"]{available}.
;Each entity has a unique id.

(test-case:
 "entity is added at location"
 (let* ([engine (make-engine 10)]
        [block (add-entity engine type-block (location 1 2))])
   (check-equal? (entity-type block) type-block)  
   (check-equal? (entity-location block) (location 1 2))
   (check-equal? (entity-by-id (engine-grid engine) (entity-id block)) block)))  

(test-case:
 "entity is not added at invalid location"
 (check-false (add-entity (make-engine 10) type-bot (location -1 2))))

(test-case:
 "entity is created with new id"
 (let ([engine (make-engine 10)])
   (check-not-equal? (entity-id (add-entity engine type-bot (location 3 4)))
                     (entity-id (add-entity engine type-bot (location 5 6))))))

;The new entity is returned, if successful.
;Otherwise @racket[#f] is returned.

(define (add-entity engine type location)
  (if (is-available? (engine-grid engine) location)
      (let ([new-entity (entity ((engine-sequence engine)) type location)])
        (place-entity (engine-grid engine) new-entity)
        new-entity)
      #f))

;The engine can @bold{move} an @bold{entity} in a direction.
;The destination of the move must be @elemref["available"]{available}.
;Otherwise, the entity remains in its original location.

(test-case:
 "move bot changes location"
 (test-engine
  ((size 10) (bot 5 6))
  (move-entity engine bot-id direction-north)
  (check-equal? (entity-location (entity-by-id (engine-grid engine) bot-id)) (location 5 7))))

(test-case:
 "invalid move leaves bot location unchanged"
 (test-engine
  ((size 10) (bot 9 9))
  (check-false (move-entity engine bot-id direction-north))
  (check-equal? (entity-location bot) (location 9 9)))) 

(test-case:
 "can not move to occupied location"
 (test-engine
  ((size 3) (bot 1 1) (block 1 2))
  (check-false (move-entity engine bot-id direction-north))
  (check-equal? (entity-location bot) (location 1 1))))

;The result is not false if successful, otherwise it is @racket[#f].

(define (move-entity engine id direction)
  (let* ([old-entity (entity-by-id (engine-grid engine) id)]
         [new-location (move-direction direction (entity-location old-entity))])
    (and (is-available? (engine-grid engine) new-location)
         (place-entity (engine-grid engine) (change-entity-location old-entity new-location)))))

;The engine can @bold{take} an @bold{entity} as cargo.
;The entity being taken must not have been removed.

(test-case:
 "block is taken"
 (test-engine
  ((size 3) (bot 1 1) (block 2 1))
  (check-not-false (take-entity engine bot-id block-id))
  (check-equal? (cargo-for-bot (engine-cargos engine) bot-id) block)
  (check-false (entity-by-id (engine-grid engine) block-id))))

(test-case:
 "can not take if block is removed"
 (test-engine
  ((size 3) (bot 1 1) (block 2 1))
  (remove-entity (engine-grid engine) block-id)
  (check-false (take-entity engine bot-id block-id))
  (check-false (cargo-for-bot (engine-cargos engine) bot-id))))

;The entity being taken is loaded into the cargos table and removed from the grid.
;The result is not false if successful, otherwise it is @racket[#f].

(define (take-entity engine id cargo-id)
  (let ([cargo (entity-by-id (engine-grid engine) cargo-id)])
    (and cargo
         (begin
           (load-cargo (engine-cargos engine) id cargo)
           (remove-entity (engine-grid engine) cargo-id)))))

;The engine can @bold{drop} an @bold{entity} that is the cargo for a bot.
;The destination of the drop must be @elemref["available"]{available}.
;Otherwise, the entity remains as cargo.

(test-case:
 "block is dropped"
 (test-engine
  ((size 3) (block 2 1) (bot 1 1))
  (take-entity engine (entity-id bot) block-id)
  (check-not-false (drop-entity engine bot-id direction-north))
  (check-false (cargo-for-bot (engine-cargos engine) bot-id))
  (check-equal? (entity-location (entity-by-id (engine-grid engine) block-id)) (location 1 2))))

(test-case:
 "can not drop in occupied location"
 (test-engine
  ((size 3) (block1 2 1) (bot 1 1) (block2 0 1))
  (take-entity engine bot-id block1-id)
  (check-false (drop-entity engine bot-id direction-west))
  (check-equal? (cargo-for-bot (engine-cargos engine) bot-id) block1)))

;The entity being dropped is unloaded from the cargos table and placed in the grid.
;The result is not false if successful, otherwise it is @racket[#f].

(define (drop-entity engine id direction)
  (let* ([bot (entity-by-id (engine-grid engine) id)]
         [drop-location (move-direction direction (entity-location bot))])
    (and (is-available? (engine-grid engine) drop-location)
         (place-entity (engine-grid engine)
                       (change-entity-location
                        (unload-cargo (engine-cargos engine) id) drop-location)))))

;The engine can @bold{transfer} an @bold{entity} from a bot to a base.
;The bot must be adjacent to the base.

(test-case:
 "transfer"
 (test-engine
  ((size 3) (bot 1 1) (base 1 2) (block 2 1))
  (take-entity engine bot-id block-id)
  (transfer-entity engine bot-id base-id)
  (check-false (cargo-for-bot (engine-cargos engine) bot-id))
  (check-equal? (cargo-for-bot (engine-cargos engine) base-id) block)))

(test-case:
 "must be adjacent"
 (test-engine
  ((size 3) (bot 1 1) (base 2 2) (block 2 1))
  (take-entity engine bot-id block-id)
  (check-false (transfer-entity engine bot-id base-id))))

;The entity being transferred is removed from the bot's cargo and added to the base's cargo.
;The result is not false if successful, otherwise it is @racket[#f].

(define (transfer-entity engine from-id to-id)
  (let ([from-entity (entity-by-id (engine-grid engine) from-id)]
        [to-entity (entity-by-id (engine-grid engine) to-id)])
    (and
     (adjacent? (entity-location from-entity) (entity-location to-entity))
     (load-cargo (engine-cargos engine) to-id
                 (unload-cargo (engine-cargos engine) from-id)))))

;The engine @bold{add}s a @bold{base at} a @bold{random} location.

(test-case:
 "add base at random"
 (test-engine
  ((size 3))
  (check-equal? (entity-location (add-base-at-random engine))
                (location 1 1))))

(define (add-base-at-random engine)
  (add-entity engine type-base (random-base (engine-grid engine))))

;The engine can provide the data that a game viewer needs to @bold{draw} the @bold{entities}.

(test-case:
 "entities are drawn"
 (test-engine
  ((size 3) (bot1 0 2) (block 2 1) (bot2 1 1))
  (check-equal? (draw-entities engine)
                (list (list type-bot #f 0 2) (list type-block #f 2 1) (list type-bot #f 1 1)))))

;The entities are provided by the grid and the cargo is retrieved from the cargos table.

(define (draw-entities engine)
  (map-entities
   (engine-grid engine)
   (λ (entity)
     (let ([cargo (cargo-for-bot (engine-cargos engine) (entity-id entity))]
           [location (entity-location entity)])
       (list (entity-type entity) (if cargo #t #f) (location-x location) (location-y location))))))
                  
;The engine provides @bold{entity info} to be returned to the client.

(test-case:
 "entity info"
 (test-engine
  ((size 4) (bot1 2 2) (block1 2 3) (block2 1 1))
  (take-entity engine bot1-id block1-id)
  (let-values ([(entity cargo neighbors) (entity-info engine bot1-id)])
    (check-equal? entity bot1)
    (check-equal? cargo block1)
    (check-equal? neighbors (list block2)))))
 
;The bot and its neighbors are retrieved from the grid and the cargo from the cargos table.

(define (entity-info engine entity-id)
  (let ([entity (entity-by-id (engine-grid engine) entity-id)])
    (values
     entity
     (cargo-for-bot (engine-cargos engine) entity-id)
     (neighbors (engine-grid engine) (entity-location entity)))))
