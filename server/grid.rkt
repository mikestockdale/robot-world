#lang racket

(provide make-grid place-entity occupant-by-id
         entity-at occupants-nearby map-entities)

(require threading "shared.rkt" "board.rkt")
(module+ test (require rackunit))

;@title{Grid}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/grid.rkt" "grid.rkt"]}
;The grid represents the 2D board where all the entities interact in the game.
;Its size is the number of rows and columns.
;The entities are stored in a hash table.
;The key is the entity id, the value is an occupant containing the entity.

(struct grid (hash))
(define (make-grid) (grid (make-hash)))

;The grid can @bold{place} an @bold{entity} in the table and retrieve the @bold{occupant by id}.

(test-case:
 "place and retrieve"
 (let ([grid (make-grid)]
       [block (entity 101 type-block)])
   (place-entity grid block (location 1 2))
   (check-equal? (occupant-by-id grid 101) (occupant block (location 1 2)))))

;This is done with basic hash table functions.

(define (occupant-by-id grid id)
  (hash-ref (grid-hash grid) id #f))

(define (place-entity grid entity location)
  (hash-set! (grid-hash grid) (entity-id entity) (occupant entity location)))

;Entities can be retrieved from the grid by location.
;We can find an @bold{entity at} at a location, and get any the @bold{occupants nearby} a location.

(test-case:
 "retrieve by location"
 (let ([grid (make-grid)]
       [block1 (entity 101 type-block)]
       [block2 (entity 102 type-block)])
   (place-entity grid block1 (location 1 2))
   (place-entity grid block2 (location 3 3))
   (place-entity grid (entity 103 type-block) (location 2 4))
   (check-false (entity-at grid 101))
   (check-equal? (entity-at grid (location 1 2)) block1)
   (check-equal? (occupants-nearby grid (location 2 2))
                 (list (occupant block1 (location 1 2))
                       (occupant block2 (location 3 3))))))

;The @racket[hash-values] function returns a list of the values in the table.
;We can then find a single instance or filter the list.

(define (entity-at grid place)
  (let ([match
            (~>> grid grid-hash hash-values
                 (findf (λ (occupant) (same-place? place (occupant-place occupant)))))])
    (if match (occupant-entity match) #f)))

(define (occupants-nearby grid location)
  (~>> grid grid-hash hash-values
       (filter (λ (occupant) (nearby-place? (occupant-place occupant) location)))))

;The grid performs a procedure on each entity to @bold{map} the @bold{entities} for a game viewer.

(test-case:
 "map all"
 (let ([grid (make-grid)])
   (place-entity grid (entity 102 type-block) (location 3 3))
   (place-entity grid (entity 103 type-block) (location 2 4))
   (check-equal?
    (map-entities grid (λ (occupant) (entity-id (occupant-entity occupant))))
    '(102 103))))

;The procedure is performed on all occupants at locations.

(define (map-entities grid procedure)
  (~>> grid grid-hash hash-values
       (filter-map (λ (item) (and (at-location? (occupant-place item)) (procedure item))))))
