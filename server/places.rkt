#lang racket

(provide make-places place-entity occupant-by-id
         entity-at occupants-nearby map-occupants map-cargos)

(require threading "shared.rkt" "board.rkt" )
(module+ test (require rackunit))

;@title{Places}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/places.rkt" "places.rkt"]}
;The grid represents the 2D board where all the entities interact in the game.
;Its size is the number of rows and columns.
;The entities are stored in a hash table.
;The key is the entity id, the value is an occupant containing the entity.

(struct places (hash))
(define (make-places) (places (make-hash)))

(struct placement (entity place) #:transparent)

;The place can be @bold{at} a @bold{location} on the game board.
;Otherwise, the place is the id of another entity that is carying the entity as cargo.

(test-case:
 "place types"
 (check-true (at-location? (location 1 2)))
 (check-false (at-location? 101)))

(define (at-location? place) (not (number? place)))

;Two places are the @bold{same place} if they have the same type and same value.

(test-case:
 "same place"
 (check-true (same-place? (location 1 1) (location 1 1)))
 (check-false (same-place? (location 1 1) (location 1 2)))
 (check-false (same-place? 101 (location 1 1)))
 (check-true (same-place? 101 101)))

(define (same-place? a b)
  (and (equal? (at-location? a) (at-location? b))
       (equal? a b)))

;The grid can @bold{place} an @bold{entity} in the table and retrieve the @bold{occupant by id}.

(test-case:
 "place and retrieve"
 (let ([places (make-places)]
       [block (entity 101 type-block)])
   (place-entity places block (location 1 2))
   (check-equal? (occupant-by-id places 101) (occupant block (location 1 2)))
   (place-entity places block 123)
   (check-false (occupant-by-id places 101))
   (check-false (occupant-by-id places 999))))

;This is done with basic hash table functions.

(define (occupant-by-id places id)
  (let ([placement (hash-ref (places-hash places) id #f)])
    (and placement (at-location? (placement-place placement))
        (occupant (placement-entity placement) (placement-place placement)))))

(define (place-entity places entity location)
  (hash-set! (places-hash places) (entity-id entity) (placement entity location)))

;Entities can be retrieved from the grid by location.
;We can find an @bold{entity at} at a location, and get any the @bold{occupants nearby} a location.

(test-case:
 "retrieve by location"
 (let ([places (make-places)]
       [block1 (entity 101 type-block)]
       [block2 (entity 102 type-block)])
   (place-entity places block1 (location 1 2))
   (place-entity places block2 (location 3 3))
   (place-entity places (entity 103 type-block) (location 2 4))
   (check-false (entity-at places 101))
   (check-equal? (entity-at places (location 1 2)) block1)
   (check-equal? (occupants-nearby places (location 2 2))
                 (list (occupant block1 (location 1 2))
                       (occupant block2 (location 3 3))))))

;The @racket[hash-values] function returns a list of the values in the table.
;We can then find a single instance or filter the list.

(define (entity-at places place)
  (let ([match
            (~>> places places-hash hash-values
                 (findf (λ (placement) (same-place? place (placement-place placement)))))])
    (if match (placement-entity match) #f)))

#;(define (occupants-nearby grid location)
  (~>> grid grid-hash hash-values
       (filter (λ (placement) (nearby-place? (placement-place placement) location)))
       (map (λ (placement) (occupantx (placement-entity placement) (placement-place placement))))))

;The grid performs a procedure on each entity to @bold{map} the @bold{entities} for a game viewer.

#;(test-case:
   "map all"
   (let ([grid (make-grid)])
     (place-entity grid (entity 102 type-block) (location 3 3))
     (place-entity grid (entity 103 type-block) (location 2 4))
     (check-equal?
      (map-entities grid (λ (occupant) (entity-id (occupant-entity occupant))))
      '(102 103))))

;The procedure is performed on all occupants at locations.

(define (map-cargos places procedure)
  (~>> places places-hash hash-values
       (filter-map (λ (item)
                     (and (not (at-location? (placement-place item)))
                          (procedure (placement-entity item) (placement-place item)))))))

(define (map-occupants places procedure)
  (~>> places places-hash hash-values
       (filter-map (λ (item)
                     (and (at-location? (placement-place item))
                          (procedure (placement-entity item) (placement-place item)))))))

(define (occupants-nearby places bot-location)
  (map-occupants
   places
   (λ (entity location)
     (and (nearby? location bot-location)
          (occupant entity location)))))
     