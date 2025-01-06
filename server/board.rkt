#lang racket

(provide board is-valid? edges random-location random-base)

(require "shared.rkt")
(module+ test (require rackunit))

;@title{Board}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/board.rkt" "board.rkt"]}
;The @bold{board} is the two-dimesional area representing the robot world.

(struct board (width height))

;@elemtag["valid"]{A location @bold{is valid} when it is part of the board.}

(test-case:
 "valid locations"
 (check-true (is-valid? (board 1 1) (location 0 0)))
 (check-true (is-valid? (board 10 10) (location 9 9)))
 (check-false (is-valid? (board 1 2) (location 0 -1)))
 (check-false (is-valid? (board 1 2) (location -1 0)))
 (check-false (is-valid? (board 10 11) (location 10 9)))
 (check-false (is-valid? (board 11 10) (location 9 10))))

;The location's x and y coordinates are checked using the size of the board.

(define (is-valid? board location)
  (define (in-range? max n) (and (>= n 0) (< n max)))
  (and (in-range? (board-width board) (location-x location))
       (in-range? (board-height board) (location-y location))))

;@bold{Edges} are entities outside the board.
;They are @elemref["adjacent"]{adjacent} to locations at the boundaries of the board.

(test-case:
 "no edges in middle"
 (check-equal? (length (edges (board 3 3) (location 1 1))) 0))

(test-case:
 "edges at limits"
 (check-equal? (length (edges (board 1 1) (location 0 0))) 4))

;We check in all directions from the
;given location and return an edge if the @elemref["adjacent"]{adjacent} location is not @elemref["valid"]{valid}.

(define (edges board location)
  (for/list ([adjacent (all-directions location)]
             #:unless (is-valid? board adjacent))
    (occupant (make-edge) adjacent)))

;A @bold{random location} is anywhere on the board.

(test-case:
 "random location"
 (let ([board (board 3 4)])
   (check-true (is-valid? board (random-location board)))))

(define (random-location board)
  (location (random (board-width board)) (random (board-height board))))

;A @bold{random base} location must have all adjacent locations available.

(test-case:
 "random base"
 (let ([board (board 4 5)])
   (define (empty location) (not (= (location-x location) 0)))
   (check-equal? (location-x (random-base board empty)) 2)))

(define (random-base board empty?)
  (let ([location (location (random 1 (sub1 (board-width board)))
                            (random 1 (sub1 (board-height board))))])
    (if (andmap empty? (all-directions location))
        location
        (random-base board empty?))))
