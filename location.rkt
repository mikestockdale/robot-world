#lang racket

(provide (struct-out location) is-valid-location? distance)

(module+ test (require rackunit))

(struct location (x y) #:transparent)

(define (is-valid-location? location size)
  (and (>= (location-x location) 0)
       (>= (location-y location) 0)
       (< (location-x location) size)
       (< (location-y location) size)))

(define (distance a b)
  (+ (abs (- (location-x a) (location-x b)))
     (abs (- (location-y a) (location-y b)))))

(module+ test
  (test-case
   "valid locations"
   (check-true (is-valid-location? (location 0 0) 1))
   (check-true (is-valid-location? (location 9 9) 10))
   (check-false (is-valid-location? (location 0 -1) 1))
   (check-false (is-valid-location? (location -1 0) 1))
   (check-false (is-valid-location? (location 10 9) 10))
   (check-false (is-valid-location? (location 9 10) 10)))

  (test-case
   "distance between locations"
   (check-equal? (distance (location 1 1) (location 1 1)) 0)
   (check-equal? (distance (location 1 1) (location 1 2)) 1)
   (check-equal? (distance (location 1 1) (location 2 1)) 1)
   (check-equal? (distance (location 3 4) (location 2 1)) 4)))
