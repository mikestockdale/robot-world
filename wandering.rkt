#lang racket

(provide make-wandering)
(require "actions.rkt" "location.rkt" "world.rkt")
(module+ test (require rackunit))

(struct wandering action (direction direction-change-chance))

(define (make-wandering world location direction [chance 0.2])
  (wandering (bot-id (add-bot! world location)) wander direction chance))

(define (wander world action)
  (define (change-direction?) (> (wandering-direction-change-chance action) (random))) 
  (let* ([old-direction (wandering-direction action)]
         [move-direction
          (if (change-direction?)
              (new-direction old-direction)
              old-direction)]
         [old-location (locate-bot world (action-bot-id action))]
         [new-location (move-bot! world (action-bot-id action) move-direction)])
    (if (equal? new-location old-location)
        (struct-copy wandering action [direction (new-direction old-direction)])
        (if (= move-direction old-direction)
            action
            (struct-copy wandering action [direction move-direction])))))

(module+ test
  (test-case
   "wandering moves in current direction"
   (let* ([world (make-world 3)]
          [action (make-wandering world (location 1 1) direction-east 0)]
          [bot-id (action-bot-id action)])
     (wander world action)
     (check-equal? (locate-bot world bot-id) (location 2 1))))
  
  (test-case
   "wandering moves in changed direction"
   (let* ([world (make-world 3)]
          [action (make-wandering world (location 1 1) direction-east 1)]
          [bot-id (action-bot-id action)]
          [new-action (wander world action)])
     (check-not-equal? (locate-bot world bot-id) (location 2 1))
     (check-not-equal? (locate-bot world bot-id) (location 1 1))
     (check-not-equal? (wandering-direction new-action) direction-east)))
  
  (test-case
   "wandering changes direction if can't move"
   (let* ([world (make-world 3)]
          [action (make-wandering world (location 2 2) direction-east 0)]
          [bot-id (action-bot-id action)]
          [new-action (wander world action)])
     (check-equal? (locate-bot world bot-id) (location 2 2))
     (check-not-equal? (wandering-direction new-action) direction-east))))