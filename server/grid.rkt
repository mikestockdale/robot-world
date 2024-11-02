#lang racket

(provide make-grid location-OK?
         place-entity remove-entity
         entity-by-id entity-at neighbors draw-each-entity)
(require threading "shared.rkt")

(struct grid (size hash))
(define (make-grid size) (grid size (make-hash)))

(define (is-valid-location? grid location)
  (let ([x (location-x location)]
        [y (location-y location)])
    (and (>= x 0)
         (>= y 0)
         (< x (grid-size grid))
         (< y (grid-size grid)))))

(define (location-OK? grid location)
  (and (is-valid-location? grid location)
       (not (entity-at grid location))))  

(define (edges grid location)
  (for/list ([direction all-directions]
             #:unless (is-valid-location? grid (move-direction direction location)))
    (make-edge (move-direction direction location))))

(define (neighbors grid location)
  (append
   (edges grid location)
   (entities-nearby grid location)))

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

(define (draw-each-entity grid procedure)
  (hash-for-each
   (grid-hash grid)
   (λ (id entity)
     (let ([location (entity-location entity)])
       (procedure entity
                  (location-x location)
                  (- (grid-size grid) 1 (location-y location)))))))

(module+ test
  (require rackunit "shared.rkt")

  (test-case
   "valid locations"
   (check-true (is-valid-location? (make-grid 1) (location 0 0)))
   (check-true (is-valid-location? (make-grid 10) (location 9 9)))
   (check-false (is-valid-location? (make-grid 1) (location 0 -1)))
   (check-false (is-valid-location? (make-grid 1) (location -1 0)))
   (check-false (is-valid-location? (make-grid 10) (location 10 9)))
   (check-false (is-valid-location? (make-grid 10) (location 9 10))))
  
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
     (let ([nearby (neighbors grid (location 1 1))])
       (check-equal? (length nearby) 1)
       (check-equal? (entity-location (first nearby)) (location 2 2)))))

  (test-case
   "neighbors include edges"
   (let* ([world (make-grid 3)])
     (let ([nearby (neighbors world (location 0 1))])
       (check-equal? (length nearby) 1)
       (check-equal? (entity-type (first nearby)) type-edge)
       (check-equal? (entity-location (first nearby)) (location -1 1)))))

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
   "draw all"
   (let ([grid (make-grid 5)]
         [count 0])
     (place-entity grid (entity 102 type-block (location 3 3)))
     (place-entity grid (entity 103 type-block (location 2 4)))
     (draw-each-entity grid (λ (entity x y) (set! count (add1 count)))) 
     (check-equal? count 2))))