#lang racket

(require "location.rkt")
(module+ test (require rackunit))

(struct bot (id location))
(struct world ([next-id #:mutable] bots))

(define (make-world) (world 101 (make-hash)))

(define (place-bot! world id location) (hash-set! (world-bots world) id location))
(define (locate-bot world id) (hash-ref (world-bots world) id))

(define (add-bot! world location)
  (let ([new-bot (bot (world-next-id world) location)])
    (place-bot! world (bot-id new-bot) (bot-location new-bot))
    (set-world-next-id! world (+ 1 (world-next-id world)))
    new-bot))

(define (move-bot! world id direction)
  (let*
      ([old-location (locate-bot world id)]
       [new-location (move-location old-location direction)])
    (place-bot! world id new-location)
    new-location))    

(module+ test
  (test-case
   "bot is created at requested location"
   (let* ([somewhere (location 1 2)]
          [new-bot (add-bot! (make-world) somewhere)])
     (check-equal? (bot-location new-bot) somewhere)))
  (test-case
   "bot is created with new id"
   (let* ([world (make-world)]
          [first-bot (add-bot! world (location 3 4))]
          [second-bot (add-bot! world (location 5 6))]          )
     (check-not-equal? (bot-id first-bot) (bot-id second-bot))))
  (test-case
   "move bot changes location"
   (let* ([world (make-world)]
          [new-bot (add-bot! world (location 5 6))])
     (check-equal? (move-bot! world (bot-id new-bot) direction-north) (location 5 7))
     (check-equal? (move-bot! world (bot-id new-bot) direction-east) (location 6 7))
     (check-equal? (move-bot! world (bot-id new-bot) direction-south) (location 6 6))
     (check-equal? (move-bot! world (bot-id new-bot) direction-west) (location 5 6))))) 
