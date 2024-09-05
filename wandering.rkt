#lang racket

(require "actions.rkt" "location.rkt" "world.rkt")
(module+ test (require rackunit))

(struct wandering action (direction direction-change-chance))

(define (wander world action)
  (let ([direction (if (< (wandering-direction-change-chance action) (random))
                       (wandering-direction action)
                       (modulo (+ (wandering-direction action) (random 1 4)) 4))])
    (move-bot! world (action-bot-id action) direction)
    action))

(module+ test
  (test-case
   "wandering moves in current direction"
   (let* ([world (make-world 3)]
          [bot-id (bot-id (add-bot! world (location 1 1)))]
          [action (wandering bot-id wander direction-east 0)])
     (wander world action)
     (check-equal? (locate-bot world bot-id) (location 2 1))))
  
  (test-case
   "wandering moves in changed direction"
   (let* ([world (make-world 3)]
          [bot-id (bot-id (add-bot! world (location 1 1)))]
          [action (wandering bot-id wander direction-east 1)])
     (wander world action)
     (check-not-equal? (locate-bot world bot-id) (location 2 1))
     (check-not-equal? (locate-bot world bot-id) (location 1 1)))))