#lang racket

(provide make-grid place-entity remove-entity entity-by-id
         entity-at is-available? neighbors map-entities)
(require threading "shared.rkt")

(struct grid (size hash))
(define (make-grid size) (grid size (make-hash)))

(define (entity-by-id grid id) (hash-ref (grid-hash grid) id #f))

(define (place-entity grid entity)
  (hash-set! (grid-hash grid) (entity-id entity) entity))

(define (remove-entity grid id)
  (hash-remove! (grid-hash grid) id))

(define (entity-at grid location)
  (~>> grid grid-hash hash-values
       (findf (λ (entity) (equal? (entity-location entity) location)))))

(define (entities-nearby grid location)
  (~>> grid grid-hash hash-values
       (filter (λ (other) (nearby? location (entity-location other))))))

(define (is-valid? grid location)
  (define (in-range? n) (and (>= n 0) (< n (grid-size grid))))
  (and (in-range? (location-x location))
       (in-range? (location-y location))))

(define (is-available? grid location)
  (and (is-valid? grid location)
       (not (entity-at grid location))))  

(define (edges grid location)
  (for/list ([direction all-directions]
             #:unless (is-valid? grid (move-direction direction location)))
    (make-edge (move-direction direction location))))

(define (neighbors grid location)
  (append
   (edges grid location)
   (entities-nearby grid location)))

(define (map-entities grid procedure)
  (hash-map (grid-hash grid)
            (λ (_ entity) (procedure entity))))

(module+ test
  (require rackunit "shared.rkt")

  (test-case
   "place and retrieve"
   (let ([grid (make-grid 5)]
         [block (entity 101 type-block (location 1 2))])
     (place-entity grid block )
     (check-equal? (entity-by-id grid 101) block)))

  (test-case
   "remove"
   (let ([grid (make-grid 5)])
     (place-entity grid (entity 101 type-block (location 1 2)))
     (remove-entity grid 101)
     (check-false (entity-by-id grid 101))))

  (test-case
   "retrieve by location"
   (let ([grid (make-grid 5)]
         [block1 (entity 101 type-block (location 1 2))]
         [block2 (entity 102 type-block (location 3 3))])
     (place-entity grid block1)
     (place-entity grid block2)
     (place-entity grid (entity 103 type-block (location 2 4)))
     (check-equal? (entity-at grid (location 1 2)) block1)
     (check-equal? (entities-nearby grid (location 2 2)) (list block1 block2))))

  (test-case
   "valid locations"
   (check-true (is-valid? (make-grid 1) (location 0 0)))
   (check-true (is-valid? (make-grid 10) (location 9 9)))
   (check-false (is-valid? (make-grid 1) (location 0 -1)))
   (check-false (is-valid? (make-grid 1) (location -1 0)))
   (check-false (is-valid? (make-grid 10) (location 10 9)))
   (check-false (is-valid? (make-grid 10) (location 9 10))))

  (test-case
   "available locations"
   (let ([grid (make-grid 3)])
     (place-entity grid (entity 101 type-block (location 1 1)))
     (check-false (is-available? grid (location 1 1)))
     (check-true (is-available? grid (location 2 1)))
     (check-false (is-available? grid (location 3 1)))))
  
  (test-case
   "no edges in middle"
   (check-equal? (length (edges (make-grid 3) (location 1 1))) 0))

  (test-case
   "edges at limits"
   (check-equal? (length (edges (make-grid 1) (location 0 0))) 4))

  (test-case
   "neighbors are nearby"
   (let* ([grid (make-grid 4)])
     (place-entity grid (entity 101 type-block (location 2 2)))
     (place-entity grid (entity 102 type-block (location 3 1)))
     (let ([neighbors (neighbors grid (location 1 1))])
       (check-equal? (length neighbors) 1)
       (check-equal? (entity-location (first neighbors)) (location 2 2)))))

  (test-case
   "neighbors include edges"
   (let* ([world (make-grid 3)])
     (let ([neighbors (neighbors world (location 0 1))])
       (check-equal? (length neighbors) 1)
       (check-equal? (entity-type (first neighbors)) type-edge)
       (check-equal? (entity-location (first neighbors)) (location -1 1)))))

  (test-case
   "map all"
   (let ([grid (make-grid 5)])
     (place-entity grid (entity 102 type-block (location 3 3)))
     (place-entity grid (entity 103 type-block (location 2 4)))
     (check-equal? (map-entities grid (λ (entity) (entity-id entity)))
                   '(102 103))))
  
  )