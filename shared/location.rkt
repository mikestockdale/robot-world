#lang racket

(provide (struct-out location) adjacent? nearby? location-offset)

(struct location (x y) #:prefab)

(define (offset field a b) (- (field a) (field b)))

(define (location-offset to from)
  (values (offset location-x to from)
          (offset location-y to from)))

(define (difference field a b) (abs (offset field a b))) 

(define (adjacent? a b)
  (= 1 (+ (difference location-x a b)
          (difference location-y a b))))

(define (nearby? a b)
  (= 1 (max (difference location-x a b)
            (difference location-y a b))))

(module+ test
  (require rackunit)

  (test-case
   "location offset"
   (define (check-offset to from expected-x expected-y) 
     (let-values ([(delta-x delta-y) (location-offset to from)])
       (check-equal? delta-x expected-x "x")
       (check-equal? delta-y expected-y "y")))
   
   (check-offset (location 3 6) (location 1 2) 2 4)
   (check-offset (location 1 2) (location 4 3) -3 -1))
  
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
