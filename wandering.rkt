#lang racket

(provide make-wandering)
(require "actions.rkt" "location.rkt" "world.rkt")
(module+ test (require rackunit))

(struct wandering action (direction direction-change-chance))

(define (make-wandering world location direction [chance 0.2])
  (wandering (add-entity! world type-bot location) wander direction chance))

(define (wander world action)
  (define (direction-change?) (> (wandering-direction-change-chance action) (random))) 
  (let* ([old-direction (wandering-direction action)]
         [move-direction
          (if (direction-change?)
              (change-direction old-direction)
              old-direction)]
         [old-location (entity-location (entity-ref world (action-bot-id action)))]
         [new-location (move-bot! world (action-bot-id action) move-direction)])
    (cond
      [(equal? new-location old-location)
       (struct-copy wandering action [direction (change-direction old-direction)])]
      [(= move-direction old-direction)
       action]
      [else
       (struct-copy wandering action [direction move-direction])])))

(module+ test
  (test-case
   "wandering moves in current direction"
   (let* ([world (make-world 3)]
          [action (make-wandering world (location 1 1) direction-east 0)]
          [bot-id (action-bot-id action)])
     (wander world action)
     (check-equal? (entity-location (entity-ref world bot-id)) (location 2 1))))
  
  (test-case
   "wandering moves in changed direction"
   (let* ([world (make-world 3)]
          [action (make-wandering world (location 1 1) direction-east 1)]
          [bot-id (action-bot-id action)]
          [new-action (wander world action)])
     (check-not-equal? (entity-location (entity-ref world bot-id)) (location 2 1))
     (check-not-equal? (entity-location (entity-ref world bot-id)) (location 1 1))
     (check-not-equal? (wandering-direction new-action) direction-east)))
  
  (test-case
   "wandering changes direction if can't move"
   (let* ([world (make-world 3)]
          [action (make-wandering world (location 2 2) direction-east 0)]
          [bot-id (action-bot-id action)]
          [new-action (wander world action)])
     (check-equal? (entity-location (entity-ref world bot-id)) (location 2 2))
     (check-not-equal? (wandering-direction new-action) direction-east))))