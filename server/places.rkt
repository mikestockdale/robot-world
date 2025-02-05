#lang racket

(provide make-places place-entity occupant-by-id place-by-id
         entity-at filter-map-occupants filter-map-cargos)

(require threading "shared.rkt")
(module+ test (require rackunit))

;@title{Places}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/places.rkt" "places.rkt"]}
;@bold{Places} are a record of where all the entities in the game are located.
;This information is stored in a hash table.
;The key is an entity id, and the value is a @racket[placement] structure containing the entity and its place.

(struct places (hash))
(define (make-places) (places (make-hash)))

(struct placement (entity place) #:transparent)

;A place can be @bold{at} a @bold{location} on the game board.
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

;We can @bold{place} an @bold{entity} in the table and retrieve an @bold{occupant by id}.

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

(define (place-entity places entity place)
  (hash-set! (places-hash places) (entity-id entity) (placement entity place)))

(define (place-by-id places id)
  (let ([placement (hash-ref (places-hash places) id #f)])
    (and placement (placement-place placement))))

;We can apply a procedure to all values in the table to @bold{filter} and @bold{map occupants} of locations.
;This can also be done to @bold{filter} and @bold{map cargos}.

(test-case:
 "map entities"
 (define (get-id entity place) (entity-id entity))
 (let ([places (make-places)])
   (place-entity places (entity 101 type-bot) (location 1 1))
   (place-entity places (entity 102 type-block) 101)
   (check-equal? (filter-map-occupants places get-id) '(101))
   (check-equal? (filter-map-cargos places get-id) '(102))))

;The values in the table are checked for the type of place and then the procedure is applied.

(define (filter-map-occupants places procedure)
  (~>> places places-hash hash-values
       (filter-map (λ (item)
                     (and (at-location? (placement-place item))
                          (procedure (placement-entity item) (placement-place item)))))))

(define (filter-map-cargos places procedure)
  (~>> places places-hash hash-values
       (filter-map (λ (item)
                     (and (not (at-location? (placement-place item)))
                          (procedure (placement-entity item) (placement-place item)))))))

;We can find an @bold{entity at} a place.

(test-case:
 "retrieve by place"
 (let ([places (make-places)]
       [block1 (entity 101 type-block)]
       [block2 (entity 102 type-block)])
   (place-entity places block1 (location 1 2))
   (place-entity places block2 104)
   (place-entity places (entity 103 type-block) (location 2 4))
   (check-false (entity-at places 101))
   (check-equal? (entity-at places (location 1 2)) block1)
   (check-equal? (entity-at places 104) block2)))

;The @racket[hash-values] function returns a list of the values in the table.
;We can then find a instance matching a place.

(define (entity-at places place)
  (let ([match
            (~>> places places-hash hash-values
                 (findf (λ (placement) (same-place? place (placement-place placement)))))])
    (and match (placement-entity match))))
