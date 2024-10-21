#lang racket

(provide location adjacent? nearby? location-offset location-coordinates location->list)

(struct location (x y) #:transparent)

(define (difference field a b)
  (abs (- (field a) (field b))))

(define (adjacent? a b)
  (= 1 (+ (difference location-x a b)
          (difference location-y a b))))

(define (nearby? a b)
  (= (max (difference location-x a b)
          (difference location-y a b))
     1))

(define (location-offset from to)
  (values (- (location-x from) (location-x to))
          (- (location-y from) (location-y to))))

(define (location-coordinates location)
  (values (location-x location) (location-y location))) 

(define (location->list location) (list (location-x location) (location-y location)))

(module+ test
  (require rackunit)
  
  (test-case
   "adjacent locations"
   (check-false (adjacent? (location 1 1) (location 1 1)))
   (check-true (adjacent? (location 1 1) (location 1 2)))
   (check-true (adjacent? (location 1 1) (location 2 1)))
   (check-false (adjacent? (location 3 4) (location 2 1))))

  (test-case
   "nearby locations"
   (check-false (nearby? (location 1 1) (location 1 1)))
   (check-true (nearby? (location 1 1) (location 1 2)))
   (check-true (nearby? (location 1 1) (location 2 2)))
   (check-false (nearby? (location 3 4) (location 2 2))))

  (test-case
   "convert to and from list"
   (check-equal? (apply location (location->list (location 3 4))) (location 3 4))))
