#lang racket

(provide (struct-out location) move-location is-valid-location? new-direction
         direction-north direction-south direction-east direction-west)

(module+ test (require rackunit))

(struct location (x y) #:transparent)

(define direction-north 0)
(define direction-east 1)
(define direction-south 2)
(define direction-west 3)

(define movement (vector (location 0 1) (location 1 0) (location 0 -1) (location -1 0)))

(define (move-location start direction)
  (let ([offset (vector-ref movement direction)])
    (location
     (+ (location-x start) (location-x offset))
     (+ (location-y start) (location-y offset)))))

(define (is-valid-location? location size)
  (and (>= (location-x location) 0)
       (>= (location-y location) 0)
       (< (location-x location) size)
       (< (location-y location) size)))

(define (new-direction current) (modulo (+ current (random 1 4)) 4))

(module+ test
  (test-case
   "location moves in all directions"
   (check-equal? (move-location (location 5 6) direction-north) (location 5 7))
   (check-equal? (move-location (location 5 6) direction-south) (location 5 5))
   (check-equal? (move-location (location 5 6) direction-east) (location 6 6))
   (check-equal? (move-location (location 5 6) direction-west) (location 4 6)))

  (test-case
   "valid locations"
   (check-true (is-valid-location? (location 0 0) 1))
   (check-true (is-valid-location? (location 9 9) 10))
   (check-false (is-valid-location? (location 0 -1) 1))
   (check-false (is-valid-location? (location -1 0) 1))
   (check-false (is-valid-location? (location 10 9) 10))
   (check-false (is-valid-location? (location 9 10) 10)))

  (test-case
   "new direction is different"
   (let ([new (new-direction direction-west)])
     (check-true (or (= new direction-north) (= new direction-south) (= new direction-east))))))
