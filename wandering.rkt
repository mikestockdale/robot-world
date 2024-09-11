#lang racket

(provide make-wandering)

(require "actions.rkt" "bot-info.rkt" "direction.rkt" "entity.rkt" "server.rkt")
(module+ test (require rackunit "location.rkt" "world.rkt"))

(struct wandering action (direction direction-change-chance take-delay))

(define (make-wandering server location direction [chance 0.2])
  (wandering (add-bot! server location) wander direction chance 0))

(define (wander server input)

  (define (find-nearby-blocks)
    (filter (λ (entity) (= (entity-type entity) type-block))
            (bot-info-neighbors (action-info input))))

  (define (take-or-drop-block blocks)
    (if (entity-cargo (action-bot input))
        (drop-block)
        (take-block (first blocks))))
  
  (define (take-block block)
    (struct-copy
     wandering input
     [info #:parent action
           (take-block! server (action-bot-id input) (entity-id block))]))

  (define (drop-block)
    (struct-copy
     wandering input
     [info #:parent action
           (drop-block! server (action-bot-id input) (find-free-direction (action-info input)))]
     [take-delay 10]))

  (define (try-to-move-bot) 
    (let* ([old-direction (wandering-direction input)]
           [move-direction (pick-direction old-direction)]
           [old-location (entity-location (action-bot input))]
           [new-info (move-bot! server (action-bot-id input) move-direction)])
      (struct-copy
       wandering input
       [info #:parent action new-info]
       [take-delay (max (- (wandering-take-delay input) 1) 0)]
       [direction (if (equal? (entity-location (bot-info-bot new-info)) old-location)
                      (change-direction old-direction)
                      move-direction)])))
  
  (define (pick-direction old-direction)
    (if (> (wandering-direction-change-chance input) (random))
        (change-direction old-direction)
        old-direction))
 
  (let ([blocks (find-nearby-blocks)])
    (if (and (= (wandering-take-delay input) 0) (> (length blocks) 0))
        (take-or-drop-block blocks)
        (try-to-move-bot))))

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
     (check-not-equal? (wandering-direction new-action) direction-east)))

  (test-case
   "wandering takes nearby block"
   (let* ([world (make-world 3)]
          [server (connect-server world)]
          [block (add-entity! world type-block (location 1 0))])
     (let* ([action (make-wandering server (location 1 1) direction-east)]
            [new-action (wander server action)])
       (check-equal? (entity-location (action-bot new-action)) (location 1 1))
       (check-equal? (entity-cargo (action-bot new-action)) block))))

  (test-case
   "wandering delays taking"
   (let* ([world (make-world 3)]
          [server (connect-server world)]
          [block (add-entity! world type-block (location 1 0))]
          [bot (add-entity! world type-block (location 1 1))]
          [action (wandering (bot-info bot (list block)) wander direction-east 0 1)]
          [new-action (wander server action)])
     (check-equal? (entity-location (action-bot new-action)) (location 2 1) "bot moved")
     (check-equal? (wandering-take-delay new-action) 0 "delay decremented")
     (check-false (entity-cargo (action-bot new-action)) "not taken")))

  (test-case
   "wandering drops nearby block"
   (let* ([world (make-world 4)]
          [server (connect-server world)]
          [block1 (add-entity! world type-block (location 1 0))]
          [bot (add-entity! world type-bot (location 2 1))]
          [block2 (add-entity! world type-block (location 3 1))]
          [laden-bot (take-entity! world (entity-id bot) (entity-id block1))]
          [action (wandering (bot-info laden-bot (list block2)) wander direction-east 0 0)]
          [new-action (wander server action)])
     (check-equal? (wandering-take-delay new-action) 10 "delay started")
     (check-equal? (entity-location (entity-ref world (entity-id block1)))
                   (location 2 2) "block dropped"))))
