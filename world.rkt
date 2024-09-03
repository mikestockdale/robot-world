#lang racket

(provide make-world add-bot! move-bot! draw-world
         bot-id)
(require "location.rkt")
(module+ test (require rackunit))

(struct bot (id location))
(struct world (size [next-id #:mutable] bots))

(define (make-world size) (world size 101 (make-hash)))

(define (place-bot! world id location) (hash-set! (world-bots world) id location))
(define (locate-bot world id) (hash-ref (world-bots world) id))

(define (add-bot! world location)
  (if (is-valid-location? location (world-size world))
      (let ([new-bot (bot (world-next-id world) location)])
        (place-bot! world (bot-id new-bot) (bot-location new-bot))
        (set-world-next-id! world (+ 1 (world-next-id world)))
        new-bot)
      #f))

(define (move-bot! world id direction)
  (let*
      ([old-location (locate-bot world id)]
       [new-location (move-location old-location direction)])
    (if (is-valid-location? new-location (world-size world))
        (begin (place-bot! world id new-location)
               new-location)
        old-location)))

(define (draw-world world)
  (let* ([size (world-size world)]
         [lines (for/vector ([_ size]) (make-string size #\space))])
    (define (draw-bot id location)
      (string-set!
       (vector-ref lines (- size 1 (location-y location)))
       (location-x location)
       #\O))
    (hash-for-each (world-bots world) draw-bot)
    lines))

(module+ test
  (test-case
   "bot is created at requested location"
   (let* ([somewhere (location 1 2)]
          [new-bot (add-bot! (make-world 10) somewhere)])
     (check-equal? (bot-location new-bot) somewhere)))

  (test-case
   "bot is not created at invalid location"
   (let* ([somewhere (location -1 2)]
          [new-bot (add-bot! (make-world 10) somewhere)])
     (check-false new-bot)))

  (test-case
   "bot is created with new id"
   (let* ([world (make-world 10)]
          [first-bot (add-bot! world (location 3 4))]
          [second-bot (add-bot! world (location 5 6))]          )
     (check-not-equal? (bot-id first-bot) (bot-id second-bot))))

  (test-case
   "move bot changes location"
   (let* ([world (make-world 10)]
          [new-bot (add-bot! world (location 5 6))])
     (check-equal? (move-bot! world (bot-id new-bot) direction-north) (location 5 7))
     (check-equal? (move-bot! world (bot-id new-bot) direction-east) (location 6 7))
     (check-equal? (move-bot! world (bot-id new-bot) direction-south) (location 6 6))
     (check-equal? (move-bot! world (bot-id new-bot) direction-west) (location 5 6))))

  (test-case
   "invalid move leaves bot location unchanged"
   (let* ([world (make-world 10)]
          [new-bot (add-bot! world (location 9 9))])
     (check-equal? (move-bot! world (bot-id new-bot) direction-north) (location 9 9)))) 

  (test-case
   "world is drawn as strings"
   (let ([world (make-world 3)])
     (add-bot! world (location 0 2))
     (add-bot! world (location 1 1))
     (add-bot! world (location 2 1))
     (check-equal? (draw-world world) #("O  " " OO" "   "))))) 
