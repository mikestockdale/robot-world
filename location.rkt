#lang racket

(provide (struct-out location) move-location
         direction-north direction-south direction-east direction-west)

(module+ test (require rackunit))

(struct location (x y) #:transparent)

(define direction-north (location 0 1))
(define direction-south (location 0 -1))
(define direction-east (location 1 0))
(define direction-west (location -1 0))

(define (move-location start offset)
  (location
   (+ (location-x start) (location-x offset))
   (+ (location-y start) (location-y offset))))

(module+ test
  (test-case
   "location moves in all directions"
   (check-equal? (move-location (location 5 6) direction-north) (location 5 7))
   (check-equal? (move-location (location 5 6) direction-south) (location 5 5))
   (check-equal? (move-location (location 5 6) direction-east) (location 6 6))
   (check-equal? (move-location (location 5 6) direction-west) (location 4 6))))
