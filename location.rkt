#lang racket

(provide (struct-out location) adjacent? nearby? location-offset)

(struct location (x y) #:prefab)

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
   (check-false (nearby? (location 3 4) (location 2 2)))))
