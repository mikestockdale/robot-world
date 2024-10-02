#lang racket

(provide (struct-out location) is-valid-location? distance nearby? location->list)

(module+ test )

(struct location (x y) #:transparent)

(define (is-valid-location? location size)
  (and (>= (location-x location) 0)
       (>= (location-y location) 0)
       (< (location-x location) size)
       (< (location-y location) size)))

(define (difference field a b)
  (abs (- (field a) (field b))))

(define (distance a b)
  (+ (difference location-x a b)
     (difference location-y a b)))

(define (nearby? a b)
  (< (max (difference location-x a b)
          (difference location-y a b))
     2))

(define (location->list location) (list (location-x location) (location-y location)))

(module+ test
  (require rackunit)
  
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
   (check-equal? (distance (location 3 4) (location 2 1)) 4))

  (test-case
   "nearby locations"
   (check-true (nearby? (location 1 1) (location 1 1)))
   (check-true (nearby? (location 1 1) (location 1 2)))
   (check-true (nearby? (location 1 1) (location 2 2)))
   (check-false (nearby? (location 3 4) (location 2 2))))

  (test-case
   "convert to and from list"
   (check-equal? (apply location (location->list (location 3 4))) (location 3 4))))
