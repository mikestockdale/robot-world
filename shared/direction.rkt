#lang racket

(provide change-direction move-direction
         direction-north direction-east direction-south direction-west
         all-directions)

(require "location.rkt" "testing.rkt")
(module+ test (require rackunit))

;@title{Direction}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/direction.rkt" "direction.rkt"]}
;A direction indicates one of the four @elemref["adjacent"]{adjacent} locations that a bot can move to from its current location.
;Directions are named after the primary compass points.

(define direction-north 0)
(define direction-east 1)
(define direction-south 2)
(define direction-west 3)

;A bot can @bold{move} in a @bold{direction} to a new location.
;Here are the results of all the possible moves from a location.

(test-case:
 "moves in all directions"
 (check-equal? (move-direction direction-north (location 5 6)) (location 5 7))
 (check-equal? (move-direction direction-south (location 5 6)) (location 5 5))
 (check-equal? (move-direction direction-east (location 5 6)) (location 6 6))
 (check-equal? (move-direction direction-west (location 5 6)) (location 4 6)))

;We define a procedure @racket[move] to create the new location.
;The @racket[movement] vector has partially-applied instances of this procedure for the four directions.
;The @racket[move-direction] procedure accesses an instance from the vector and applies the third argument.

(define ((move delta-x delta-y) from)
  (location (+ (location-x from) delta-x)
            (+ (location-y from) delta-y)))

(define movement (vector (move 0 1) (move 1 0) (move 0 -1) (move -1 0)))

(define (move-direction direction from)
  ((vector-ref movement direction) from))

;We can list the locations in @bold{all directions} from a location.

(test-case:
 "all directions from location"
 (check-equal? (all-directions (location 1 2))
               (list (location 1 3) (location 2 2) (location 1 1) (location 0 2))))

;Each item in the @racket[movement] vector is appplied to the location.

(define (all-directions location)
  (map (λ (move) (move location)) (vector->list movement)))

;A bot needs to @bold{change direction} when the current direction is blocked.

(test-case:
 "new direction is different"
 (let ([new (change-direction direction-west)])
   (check-true
    (or (= new direction-north) (= new direction-south) (= new direction-east)))))

;A simple way to change direction is to pick another direction at random.

(define (change-direction current) (modulo (+ current (random 1 4)) 4))
