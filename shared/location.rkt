#lang racket/base

(provide location location-x location-y adjacent? nearby? location-offset)
(require "testing.rkt")
(module+ test (require rackunit))

;@title{Location}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/location.rkt" "location.rkt"]}

;A @bold{location} in the 2-D grid world is identified by two non-negative integer coordinates.

(struct location (x y) #:prefab)

;The @elemtag["location-offset"]{@bold{location offset}} from one location to another is the differences between their x-coordinates and between their y-coordinates.
;Here's a test that shows a couple of examples:
;@margin-note{@racket[test-case:] is a macro that makes it easier to interleave test and production code: source at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/testing.rkt" "testing.rkt"]}

(test-case:
 "location offset"
 (define (check-offset to from expected-x expected-y) 
   (let-values ([(delta-x delta-y) (location-offset to from)])
     (check-equal? delta-x expected-x "x")
     (check-equal? delta-y expected-y "y")))
 (check-offset (location 3 6) (location 1 2) 2 4)
 (check-offset (location 1 2) (location 4 3) -3 -1))
 
;In the helper procedure @racket[check-offset], we get the two values returned from @racket[location-offset] and check them with the expected values.

;The implementation is simple.

(define (offset field a b) (- (field a) (field b)))

(define (location-offset to from)
  (values (offset location-x to from)
          (offset location-y to from)))

;Two locations may be @elemref["adjacent"]{adjacent} to each other. Here are some examples of adjacent and non-adjacent locations:

(test-case:
 "adjacent locations"
 (check-false (adjacent? (location 1 1) (location 1 1)))
 (check-true (adjacent? (location 1 1) (location 1 2)))
 (check-true (adjacent? (location 1 1) (location 2 1)))
 (check-false (adjacent? (location 3 4) (location 2 1))))

;Locations are adjacent when the difference of the x-coordinates and difference of the y-coordinates add up to 1.

(define (difference field a b) (abs (offset field a b))) 

(define (adjacent? a b)
  (= 1 (+ (difference location-x a b)
          (difference location-y a b))))


;Two locations may be @elemref["nearby"]{nearby}. Here are some examples:

(test-case:
 "nearby locations"
 (check-false (nearby? (location 1 1) (location 1 1)))
 (check-true (nearby? (location 1 1) (location 1 2)))
 (check-true (nearby? (location 1 1) (location 2 2)))
 (check-false (nearby? (location 3 4) (location 2 2))))

;Locations are nearby when the maximum of the difference of the x-coordinates and the difference of the y-coordinates is 1.

(define (nearby? a b)
  (= 1 (max (difference location-x a b)
            (difference location-y a b))))
