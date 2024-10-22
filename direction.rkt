#lang racket

(provide change-direction move-direction direction-from direction-towards
         direction-north direction-south direction-east direction-west
         all-directions filter-map-directions)

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
  (let-values ([(x y) (location-coordinates from)])
    (let ([offset (vector-ref movement direction)])
      (location
       (+ x (offset-delta-x offset))
       (+ y (offset-delta-y offset))))))

(define (change-direction current) (modulo (+ current (random 1 4)) 4))

(define (direction-from from to)
  (findf (λ (direction) (equal? (move-direction direction from) to)) all-directions))

(define (direction-towards from to)
  (let-values ([(difference-x difference-y) (location-offset from to)])
    (if (> (abs difference-x) (abs difference-y))
        (if (positive? difference-x) direction-west direction-east)
        (if (positive? difference-y) direction-south direction-north))))

(define (filter-map-directions proc) (filter-map proc all-directions))

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
   "direction from location to location"
   (check-equal? (direction-from (location 1 1) (location 2 1)) direction-east)
   (check-false (direction-from (location 1 1) (location 2 2))))

  (test-case
   "direction towards"
   (check-equal? (direction-towards (location 1 1) (location 3 4)) direction-north)
   (check-equal? (direction-towards (location 1 1) (location 4 3)) direction-east)
   (check-equal? (direction-towards (location 1 4) (location 3 1)) direction-south)
   (check-equal? (direction-towards (location 4 1) (location 1 3)) direction-west)))
