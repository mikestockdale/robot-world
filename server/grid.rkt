#lang racket

(provide make-grid place-entity remove-entity
         entity-by-id entity-at entities-nearby for-each-entity)
(require threading "shared.rkt")

(struct grid (hash))
(define (make-grid) (grid (make-hash)))

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

(define (for-each-entity grid procedure)
  (hash-for-each (grid-hash grid) procedure))

(module+ test
  (require rackunit "shared.rkt")

  (test-case
   "place and retrieve"
   (let ([grid (make-grid)]
         [block (entity 101 type-block (location 1 2))])
     (place-entity grid block )
     (check-equal? (entity-by-id grid 101) block)))

  (test-case
   "remove"
   (let ([grid (make-grid)])
     (place-entity grid (entity 101 type-block (location 1 2)))
     (remove-entity grid 101)
     (check-false (entity-by-id grid 101))))

  (test-case
   "retrieve by location"
   (let ([grid (make-grid)]
         [block1 (entity 101 type-block (location 1 2))]
         [block2 (entity 102 type-block (location 3 3))])
     (place-entity grid block1)
     (place-entity grid block2)
     (place-entity grid (entity 103 type-block (location 2 4)))
     (check-equal? (entity-at grid (location 1 2)) block1)
     (check-equal? (entities-nearby grid (location 2 2)) (list block1 block2))))

  (test-case
   "process all"
   (let ([grid (make-grid)]
         [count 0])
     (place-entity grid (entity 102 type-block (location 3 3)))
     (place-entity grid (entity 103 type-block (location 2 4)))
     (for-each-entity grid (λ (id entity) (set! count (add1 count)))) 
     (check-equal? count 2))))