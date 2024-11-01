#lang racket

(provide make-grid place-in-grid! remove-from-grid!
         entity-ref at-grid-location nearby-grid-location grid-for-each)
(require threading "shared.rkt")

(define (make-grid) (make-hash))

(define (entity-ref grid id) (hash-ref grid id #f))

(define (grid-for-each grid procedure)
  (hash-for-each grid procedure))

(define (at-grid-location grid location)
  (~>> grid hash-values
       (findf (λ (entity) (equal? (entity-location entity) location)))))

(define (nearby-grid-location grid location)
   (~>> grid hash-values
        (filter (λ (other) (nearby? location (entity-location other))))))

(define (place-in-grid! grid entity)
  (hash-set! grid (entity-id entity) entity))

(define (remove-from-grid! grid id)
  (hash-remove! grid id))

