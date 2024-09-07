#lang racket

(provide make-wandering)
(require "actions.rkt" "entity.rkt" "location.rkt" "server.rkt")
(module+ test (require rackunit))

(struct wandering action (direction direction-change-chance))

(define (make-wandering server location direction [chance 0.2])
  (wandering (add-bot! server location) wander direction chance))

(define (wander server input)
  (define (direction-change?) (> (wandering-direction-change-chance input) (random))) 
  (let* ([old-direction (wandering-direction input)]
         [move-direction
          (if (direction-change?)
              (change-direction old-direction)
              old-direction)]
         [old-location (entity-location (action-bot input))]
         [new-info (move-bot! server (entity-id (action-bot input)) move-direction)])
    (if
     (equal? (entity-location (info-bot new-info)) old-location)
     (struct-copy wandering input [direction (change-direction old-direction)])
     (struct-copy wandering input [info #:parent action new-info] [direction move-direction]))))

(module+ test
  (test-case
   "wandering moves in current direction"
   (let* ([server (make-server 3)]
          [action (make-wandering server (location 1 1) direction-east 0)])
     (wander server action)
     (check-equal? (entity-location (action-bot (wander server action))) (location 2 1))))
  
  (test-case
   "wandering moves in changed direction"
   (let* ([server (make-server 3)]
          [action (make-wandering server (location 1 1) direction-east 1)]
          [new-action (wander server action)])
     (check-not-equal? (entity-location (action-bot new-action)) (location 2 1))
     (check-not-equal? (entity-location (action-bot new-action)) (location 1 1))
     (check-not-equal? (wandering-direction new-action) direction-east)))
  
  (test-case
   "wandering changes direction if can't move"
   (let* ([server (make-server 3)]
          [action (make-wandering server (location 2 2) direction-east 0)]
          [new-action (wander server action)])
     (check-equal? (entity-location (action-bot new-action)) (location 2 2))
     (check-not-equal? (wandering-direction new-action) direction-east))))