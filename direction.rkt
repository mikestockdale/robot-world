#lang racket

(provide change-direction move-direction
         direction-north direction-south direction-east direction-west find-direction)

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

(define (find-direction filter) (findf filter all-directions))

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
                 direction-south)))
