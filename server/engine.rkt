#lang racket

(provide make-engine entity-info draw-entities
         add-entity move-entity take-entity drop-entity)

(require threading)
(require "shared.rkt" "cargos.rkt" "grid.rkt" "sequence.rkt")
(module+ test (require rackunit))

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
      (let ([new-entity (entity (new-id (engine-sequence engine)) type location)])
        (place-entity (engine-grid engine) new-entity)
        new-entity)
      #f))

;The engine can @bold{move} an @bold{entity} in a direction.
;The destination of the move must be @elemref["available"]{available}.
;Otherwise, the entity remains in its original location.

(test-case:
 "move bot changes location"
 (let* ([engine (make-engine 10)]
        [bot (add-entity engine type-bot (location 5 6))]
        [id (entity-id bot)])
   (move-entity engine id direction-north)
   (check-equal? (entity-location (entity-by-id (engine-grid engine) id)) (location 5 7))))

(test-case:
 "invalid move leaves bot location unchanged"
 (let* ([engine (make-engine 10)]
        [bot (add-entity engine type-bot (location 9 9))])
   (check-false (move-entity engine (entity-id bot) direction-north))
   (check-equal? (entity-location bot) (location 9 9)))) 

(test-case:
 "can not move to occupied location"
 (let* ([engine (make-engine 3)]
        [bot (add-entity engine type-bot (location 1 1))])
   (add-entity engine type-block (location 1 2))
   (check-false (move-entity engine (entity-id bot) direction-north))
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
 (let* ([engine (make-engine 3)]
        [bot (add-entity engine type-bot (location 1 1))]
        [block (add-entity engine type-block (location 2 1))])
   (check-not-false (take-entity engine (entity-id bot) (entity-id block)))
   (check-equal? (cargo-for-bot (engine-cargos engine) (entity-id bot)) block)
   (check-false (entity-by-id (engine-grid engine) (entity-id block)))))

(test-case:
 "can not take if block is removed"
 (let* ([engine (make-engine 3)]
        [bot (add-entity engine type-bot (location 1 1))]
        [block (add-entity engine type-block (location 2 1))])
   (remove-entity (engine-grid engine) (entity-id block))
   (check-false (take-entity engine (entity-id bot) (entity-id block)))
   (check-false (cargo-for-bot (engine-cargos engine) (entity-id bot)))))

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
 (let* ([engine (make-engine 3)]
        [block (add-entity engine type-block (location 2 1))]
        [bot (add-entity engine type-bot (location 1 1))])
   (take-entity engine (entity-id bot) (entity-id block))
   (check-not-false (drop-entity engine (entity-id bot) direction-north))
   (check-false (cargo-for-bot (engine-cargos engine) (entity-id bot)))
   (check-equal? (entity-location (entity-by-id (engine-grid engine) (entity-id block))) (location 1 2))))

(test-case:
 "can not drop in occupied location"
 (let* ([engine (make-engine 3)]
        [block (add-entity engine type-block (location 2 1))]
        [bot (add-entity engine type-bot (location 1 1))])
   (take-entity engine (entity-id bot) (entity-id block))
   (add-entity engine type-block (location 0 1))
   (check-false (drop-entity engine (entity-id bot) direction-west))
   (check-equal? (cargo-for-bot (engine-cargos engine) (entity-id bot)) block)))

;The entity being dropped is unloaded from the cargos table and placed in the grid.
;The result is not false if successful, otherwise it is @racket[#f].

(define (drop-entity engine id direction)
  (let* ([bot (entity-by-id (engine-grid engine) id)]
         [drop-location (move-direction direction (entity-location bot))])
    (and (is-available? (engine-grid engine) drop-location)
         (place-entity (engine-grid engine)
                       (change-entity-location
                        (unload-cargo (engine-cargos engine) id) drop-location)))))

;The engine can @bold{draw} the @bold{entities} by calling a procedure from a game viewer.

(test-case:
 "entities are drawn"
 (let* ([engine (make-engine 3)]
        [bot (entity-symbol (add-entity engine type-bot (location 0 2)) #f)]
        [block (entity-symbol (add-entity engine type-block (location 2 1)) #f)])
   (add-entity engine type-bot(location 1 1))
   (check-equal? (draw-entities engine) (list (list bot 0 2) (list block 2 1) (list bot 1 1)))))

;The entities are provided by the grid and the cargo is retrieved from the cargos table.

(define (draw-entities engine)
  (map-entities
   (engine-grid engine)
   (λ (entity)
     (let ([cargo (cargo-for-bot (engine-cargos engine) (entity-id entity))]
           [location (entity-location entity)])
       (list (entity-symbol entity cargo) (location-x location) (location-y location))))))
                  
;The engine provides @bold{entity info} to be returned to the client.

(test-case:
 "entity info"
 (let* ([engine (make-engine 4)]
        [bot1 (add-entity engine type-bot (location 2 2))]
        [block1 (add-entity engine type-block (location 2 3))]
        [block2 (add-entity engine type-block (location 1 1))])
   (take-entity engine (entity-id bot1) (entity-id block1))
   (let-values ([(entity cargo neighbors) (entity-info engine (entity-id bot1))])
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
