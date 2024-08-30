#lang racket

(module+ test (require rackunit))

(struct location (x y) #:transparent)
(struct bot (id location))
(struct world (bots))

; server
(define start-location (location 10 10))
(define new-id 101)
(define (add-bot! world)
  (let ([new-bot (bot new-id start-location)])
    (hash-set! (world-bots world) (bot-id new-bot) (bot-location new-bot))
    new-bot))
(define (move-bot! world id delta-x delta-y)
  (let*
      ([old-location (hash-ref (world-bots world) id)]
       [new-location (location
                      (+ (location-x old-location) delta-x)
                      (+ (location-y old-location) delta-y))])
    (hash-set! (world-bots world) id new-location)
    new-location))    

; client
(define (connect-world) (world (make-hash)))
(define (make-bot world) (add-bot! world))
(define (move-bot world id delta-x delta-y) (move-bot! world id delta-x delta-y))

(module+ test
  (test-case
   "bot is created at start location"
   (check-equal? (bot-location (make-bot (connect-world))) start-location))
  (test-case
   "bot is created with new id"
   (check-equal? (bot-id (make-bot (connect-world))) new-id))
  (test-case
   "move bot changes location"
   (let ([world (connect-world)])
     (check-equal? (move-bot world (bot-id (make-bot world)) -2 3) (location 8 13))))) 
