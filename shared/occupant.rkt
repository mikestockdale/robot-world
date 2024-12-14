#lang racket/base

(provide (struct-out occupant) at-location? same-place? nearby-place?)
(require "location.rkt" "testing.rkt")
(module+ test (require rackunit))

;@title{Occupant}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/occupant.rkt" "occupant.rkt"]}
;Each entity occupies a place in the game.

(struct occupant (entity place) #:prefab)

;The place can be @bold{at} a @bold{location} on the game board, or else as cargo for another entity.

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

;A place can be a @bold{nearby place} when it is at a location.

(test-case:
 "nearby place"
 (check-true (nearby-place? (location 1 1) (location 2 2)))
 (check-false (nearby-place? 101 (location 2 2)))
 (check-false (nearby-place? (location 1 1) (location 9 9))))

(define (nearby-place? place location)
  (and (at-location? place)
       (nearby? location place)))
