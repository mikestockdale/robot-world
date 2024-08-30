#lang racket

(module+ test (require rackunit))

(struct location (x y) #:transparent)
(define (move-location start delta-x delta-y)
  (location
   (+ (location-x start) delta-x)
   (+ (location-y start) delta-y)))

(struct bot (id location))
(struct world (bots))

(define (make-world) (world (make-hash)))

(define (place-bot! world id location) (hash-set! (world-bots world) id location))
(define (locate-bot world id) (hash-ref (world-bots world) id))

(define start-location (location 10 10))
(define new-id 101)

(define (add-bot! world)
  (let ([new-bot (bot new-id start-location)])
    (place-bot! world (bot-id new-bot) (bot-location new-bot))
    new-bot))

(define (move-bot! world id delta-x delta-y)
  (let*
      ([old-location (locate-bot world id)]
       [new-location (move-location old-location delta-x delta-y)])
    (place-bot! world id new-location)
    new-location))    

(module+ test
  (test-case
   "bot is created at start location"
   (check-equal? (bot-location (add-bot! (make-world))) start-location))
  (test-case
   "bot is created with new id"
   (check-equal? (bot-id (add-bot! (make-world))) new-id))
  (test-case
   "move bot changes location"
   (let ([world (make-world)])
     (check-equal? (move-bot! world (bot-id (add-bot! world)) -2 3) (location 8 13))))) 
