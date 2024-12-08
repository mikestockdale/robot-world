#lang racket

(provide make-grid place-entity remove-entity occupant-by-id
         entity-at is-available? neighbors map-entities random-base)

(require threading "shared.rkt")
(module+ test (require rackunit))

;@title{Grid}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/grid.rkt" "grid.rkt"]}
;The grid represents the 2D board where all the entities interact in the game.
;Its size is the number of rows and columns.
;The entities are stored in a hash table.
;The key is the entity id, the value is the entity.

(struct grid (size hash))
(define (make-grid size) (grid size (make-hash)))

;The grid can @bold{place} an @bold{entity} in the table and retrieve the @bold{occupant by id}.

(test-case:
 "place and retrieve"
 (let ([grid (make-grid 5)]
       [block (entity 101 type-block)])
   (place-entity grid block (location 1 2))
   (check-equal? (occupant-by-id grid 101) (occupant block (location 1 2)))))

;This is done with basic hash table functions.

(define (occupant-by-id grid id)
  (hash-ref (grid-hash grid) id #f))

(define (place-entity grid entity location)
  (hash-set! (grid-hash grid) (entity-id entity) (occupant entity location)))

;The grid can also @bold{remove} an @bold{entity} from the table.

(test-case:
 "remove"
 (let ([grid (make-grid 5)])
   (place-entity grid (entity 101 type-block) (location 1 2))
   (remove-entity grid 101)
   (check-false (occupant-by-id grid 101))))

(define (remove-entity grid id)
  (hash-remove! (grid-hash grid) id))

;Entities can be retrieved from the grid by location.
;We can find an @bold{entity at} at a location, and get any the @bold{entities nearby} a location.

(test-case:
 "retrieve by location"
 (let ([grid (make-grid 5)]
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
  (let ([match (~>> grid grid-hash hash-values
                    (findf (λ (occupant)
                             (and (equal? (number? place) (number? (occupant-place occupant)))
                                  (equal? (occupant-place occupant) place)))))])
    (if match (occupant-entity match) #f)))

(define (occupants-nearby grid location)
  (~>> grid grid-hash hash-values
       (filter (λ (other)
                 (and (not (number? (occupant-place other)))
                      (nearby? location (occupant-place other)))))))

;@elemtag["valid"]{A location @bold{is valid} when it is part of the grid.}

(test-case:
 "valid locations"
 (check-true (is-valid? (make-grid 1) (location 0 0)))
 (check-true (is-valid? (make-grid 10) (location 9 9)))
 (check-false (is-valid? (make-grid 1) (location 0 -1)))
 (check-false (is-valid? (make-grid 1) (location -1 0)))
 (check-false (is-valid? (make-grid 10) (location 10 9)))
 (check-false (is-valid? (make-grid 10) (location 9 10))))

;The location's x and y coordinates are checked using the size of the grid.

(define (is-valid? grid location)
  (define (in-range? n) (and (>= n 0) (< n (grid-size grid))))
  (and (in-range? (location-x location))
       (in-range? (location-y location))))

;@elemtag["available"]{A location @bold{is available} when it is @elemref["valid"]{valid} and there is no entity at that location}.

(test-case:
 "available locations"
 (let ([grid (make-grid 3)])
   (place-entity grid (entity 101 type-block) (location 1 1))
   (check-false (is-available? grid (location 1 1)))
   (check-true (is-available? grid (location 2 1)))
   (check-false (is-available? grid (location 3 1)))))


(define (is-available? grid location)
  (and (is-valid? grid location)
       (not (entity-at grid location))))  

;@bold{Edges} are entities outside the grid.
;They are @elemref["adjacent"]{adjacent} to locations at the boundaries of the grid.

(test-case:
 "no edges in middle"
 (check-equal? (length (edges (make-grid 3) (location 1 1))) 0))

(test-case:
 "edges at limits"
 (check-equal? (length (edges (make-grid 1) (location 0 0))) 4))

;We check in all directions from the
;given location and return an edge if the @elemref["adjacent"]{adjacent} location is not @elemref["valid"]{valid}.

(define (edges grid location)
  (for/list ([adjacent (all-directions location)]
             #:unless (is-valid? grid adjacent))
    (occupant (make-edge) adjacent)))

;@bold{Neighbors} of a location are all the nearby entites, plus any edges.

(test-case:
 "neighbors are nearby"
 (let* ([grid (make-grid 4)])
   (place-entity grid (entity 101 type-block) (location 2 2))
   (place-entity grid (entity 102 type-block) (location 3 1))
   (let ([neighbors (neighbors grid (location 1 1))])
     (check-equal? (length neighbors) 1)
     (check-equal? (occupant-place (first neighbors)) (location 2 2)))))

(test-case:
 "neighbors include edges"
 (let* ([grid (make-grid 3)]
        [neighbors (neighbors grid (location 0 1))])
   (check-equal? (length neighbors) 1)
   (check-equal? (entity-type (occupant-entity (first neighbors))) type-edge)
   (check-equal? (occupant-place (first neighbors)) (location -1 1))))

(define (neighbors grid location)
  (append
   (edges grid location)
   (occupants-nearby grid location)))

;The grid performs a procedure on each entity to @bold{map} the @bold{entities} for a game viewer.

(test-case:
 "map all"
 (let ([grid (make-grid 5)])
   (place-entity grid (entity 102 type-block) (location 3 3))
   (place-entity grid (entity 103 type-block) (location 2 4))
   (check-equal?
    (map-entities grid (λ (occupant) (entity-id (occupant-entity occupant))))
    '(102 103))))

;This is another wrapper on a hash table function.

(define (map-entities grid procedure)
  (~>> grid grid-hash hash-values
       (filter (λ (item) (not (number? (occupant-place item)))))
       (map procedure)))

;The grid selects a @bold{random base} location.
;The location must have all adjacent locations available.

(test-case:
 "random base"
 (let ([grid (make-grid 4)])
   (place-entity grid (entity 101 type-block) (location 1 0))
   (check-not-equal? (random-base grid) (location 1 1))))

(define (random-base grid)
  (let* ([top (sub1 (grid-size grid))]
         [location (location (random 1 top) (random 1 top))])
    (if (andmap (λ (x) (is-available? grid x))
                (all-directions location))
        location
        (random-base grid))))
