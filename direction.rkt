#lang racket

(provide change-direction move-direction direction-from find-direction left-of right-of
         direction-north direction-south direction-east direction-west
         all-directions)

(require "location.rkt")
(module+ test (require rackunit))

(struct offset (delta-x delta-y))

(define direction-north 0)
(define direction-east 1)
(define direction-south 2)
(define direction-west 3)

(define all-directions (list direction-north direction-east direction-south direction-west))

(define movement (vector (offset 0 1) (offset 1 0) (offset 0 -1) (offset -1 0)))

(define (move-direction direction from)
  (let ([offset (vector-ref movement direction)])
    (location
     (+ (location-x from) (offset-delta-x offset))
     (+ (location-y from) (offset-delta-y offset)))))

(define (change-direction current) (modulo (+ current (random 1 4)) 4))

(define (left-of direction) (modulo (+ direction 3) 4))
(define (right-of direction) (modulo (+ direction 1) 4))

(define (find-direction filter) (findf filter all-directions))

(define (direction-from from to)
  (find-direction (λ (direction) (equal? (move-direction direction from) to))))

(module+ test
  (test-case
   "location moves in all directions"
   (check-equal? (move-direction direction-north (location 5 6)) (location 5 7))
   (check-equal? (move-direction direction-south (location 5 6)) (location 5 5))
   (check-equal? (move-direction direction-east (location 5 6)) (location 6 6))
   (check-equal? (move-direction direction-west (location 5 6)) (location 4 6)))
  
  (test-case
   "new direction is different"
   (let ([new (change-direction direction-west)])
     (check-true (or (= new direction-north) (= new direction-south) (= new direction-east)))))

  (test-case
   "direction is found"
   (check-equal? (find-direction
                  (λ (direction) (equal? (move-direction direction (location 1 1)) (location 1 0))))
                 direction-south))

  (test-case
   "left of direction"
   (check-equal? (left-of direction-north) direction-west)
   (check-equal? (left-of direction-east) direction-north)
   (check-equal? (left-of direction-south) direction-east)
   (check-equal? (left-of direction-west) direction-south))

  (test-case
   "right of direction"
   (check-equal? (right-of direction-north) direction-east)
   (check-equal? (right-of direction-east) direction-south)
   (check-equal? (right-of direction-south) direction-west)
   (check-equal? (right-of direction-west) direction-north))

  (test-case
   "direction from location to location"
   (check-equal? (direction-from (location 1 1) (location 2 1)) direction-east)
   (check-false (direction-from (location 1 1) (location 2 2)))))
