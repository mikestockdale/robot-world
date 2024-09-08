#lang racket

(provide make-wandering)
(require "actions.rkt" "entity.rkt" "location.rkt" "server.rkt")
(module+ test (require rackunit "world.rkt"))

(struct wandering action (direction direction-change-chance))

(define (make-wandering server location direction [chance 0.2])
  (wandering (add-bot! server location) wander direction chance))

(define (wander server input)

  (define (find-nearby-blocks)
    (filter (λ (entity) (= (entity-type entity) type-block))
            (info-neighbors (action-info input))))

  (define (load-block block)
    (struct-copy
     wandering input
     [info #:parent action
           (load-block! server (action-bot-id input) (entity-id block))]))
    
  (define (pick-direction old-direction)
    (if (> (wandering-direction-change-chance input) (random))
        (change-direction old-direction)
        old-direction))

  (define (try-to-move-bot) 
    (let* ([old-direction (wandering-direction input)]
           [move-direction (pick-direction old-direction)]
           [old-location (entity-location (action-bot input))]
           [new-info (move-bot! server (action-bot-id input) move-direction)])
      (struct-copy
       wandering input
       [info #:parent action new-info]
       [direction (if (equal? (entity-location (info-bot new-info)) old-location)
                      (change-direction old-direction)
                      move-direction)])))      
  
  (let ([blocks (find-nearby-blocks)])
    (if (> (length blocks) 0)
        (load-block (first blocks))
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
   "wandering loads nearby block"
   (let* ([world (make-world 3)]
          [server (connect-server world)]
          [block (add-entity! world type-block (location 1 0))])
     (let* ([action (make-wandering server (location 1 1) direction-east)]
            [new-action (wander server action)])
       (check-equal? (entity-location (action-bot new-action)) (location 1 1))
       (check-equal? (entity-cargo (action-bot new-action)) block)))))
