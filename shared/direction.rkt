#lang racket

(provide move-direction direction-from
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

;We can list the locations in @bold{all directions} from a source location.
;The @racket[#:except] keyword excludes a direction.

(test-case:
 "all directions from location"
 (check-equal? (all-directions (location 1 2))
               (list (location 1 3) (location 2 2) (location 1 1) (location 0 2)))
 (check-equal? (all-directions (location 1 2) #:except direction-east)
               (list (location 1 3) (location 1 1) (location 0 2))))

;Each item is a movement from the source location.

(define (all-directions source #:except [except -1])
  (for/list ([direction 4] #:when (not (= direction except)))
    (move-direction direction source)))

;The @bold{direction from} one location to another is a direction that will move a bot closer to a destination.

(test-case:
 "direction from location to location"
 (check-equal? (direction-from (location 1 1) (location 2 1)) direction-east)
 (check-equal? (direction-from (location 1 1) (location 3 4)) direction-north)
 (check-equal? (direction-from (location 1 1) (location 4 3)) direction-east)
 (check-equal? (direction-from (location 1 4) (location 3 1)) direction-south)
 (check-equal? (direction-from (location 4 1) (location 1 3)) direction-west))

;The direction returned will reduce the larger of the x and y difference.

(define (direction-from from to)
  (let-values ([(difference-x difference-y) (location-offset from to)])
    (if (> (abs difference-x) (abs difference-y))
        (if (positive? difference-x) direction-west direction-east)
        (if (positive? difference-y) direction-south direction-north))))
