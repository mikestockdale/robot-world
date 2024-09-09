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

  (define (take-or-drop-block blocks)
    (if (entity-cargo (action-bot input))
        (drop-block blocks)
        (take-block (first blocks))))
  
  (define (take-block block)
    (struct-copy
     wandering input
     [info #:parent action
           (take-block! server (action-bot-id input) (entity-id block))]))

  (define (drop-block blocks)
    (struct-copy
     wandering input
     [info #:parent action
           (drop-block! server (action-bot-id input) (find-direction blocks))]))

  (define (find-direction blocks)
    (findf (λ (direction)
             (is-free? (move-location (entity-location (action-bot input)) direction)
                       blocks))
           all-directions))

  (define (is-free? location blocks)
    (not (findf (λ (block) (equal? location (entity-location block))) blocks)))
    
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
  
   (define (pick-direction old-direction)
    (if (> (wandering-direction-change-chance input) (random))
        (change-direction old-direction)
        old-direction))
 
  (let ([blocks (find-nearby-blocks)])
    (if (> (length blocks) 0)
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
   "wandering drops nearby block"
   (let* ([world (make-world 4)]
          [server (connect-server world)]
          [block1 (add-entity! world type-block (location 1 0))]
          [bot (add-entity! world type-bot (location 2 1))]
          [block2 (add-entity! world type-block (location 3 1))]
          [laden-bot (take-entity! world (entity-id bot) (entity-id block1))]
          [action (wandering (info laden-bot (list block2)) wander direction-east 0)])
     (wander server action)
     (check-equal? (entity-location (entity-ref world (entity-id block1))) (location 2 2)))))
