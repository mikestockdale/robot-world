#lang racket

(module+ test (require rackunit))

(struct location (x y) #:transparent)
(define (move-location start delta-x delta-y)
  (location
   (+ (location-x start) delta-x)
   (+ (location-y start) delta-y)))

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

(define (move-bot! world id delta-x delta-y)
  (let*
      ([old-location (locate-bot world id)]
       [new-location (move-location old-location delta-x delta-y)])
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
          [second-bot (add-bot! world (location 5 6))]
          )
     (check-not-equal? (bot-id first-bot) (bot-id second-bot))))
  (test-case
   "move bot changes location"
   (let* ([world (make-world)]
          [new-bot (add-bot! world (location 5 6))])
     (check-equal? (move-bot! world (bot-id new-bot) -2 3) (location 3 9))))) 
