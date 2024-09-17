#lang racket

(provide make-wandering)

(require "actions.rkt" "bot-info.rkt" "direction.rkt" "entity.rkt" "server.rkt")

(struct wandering action (direction direction-change-chance take-delay))

(define (make-wandering server location direction [chance 0.2])
  (wandering (add-bot! server location) wander direction chance 0))

(define (wander server input)

  (define (take-or-drop-block blocks)
    (if (entity-cargo (action-bot input))
        (drop-block)
        (take-block (first blocks))))
  
  (define (take-block block)
    (result (take-block! server (action-bot-id input) (entity-id block))))

  (define (drop-block)
    (result (drop-block! server (action-bot-id input) (find-free-direction (action-info input)))
            #:take-delay 10))

  (define (try-to-move-bot) 
    (let* ([move-direction (pick-direction (wandering-direction input))]
           [old-location (entity-location (action-bot input))]
           [new-info (move-bot! server (action-bot-id input) move-direction)])
      (result new-info 
              #:take-delay (max (- (wandering-take-delay input) 1) 0)
              #:direction (if (equal? (entity-location (bot-info-bot new-info)) old-location)
                              (change-direction move-direction)
                              move-direction))))
  
  (define (pick-direction old-direction)
    (if (> (wandering-direction-change-chance input) (random))
        (change-direction old-direction)
        old-direction))

  (define (result new-info
                  #:take-delay [take-delay (wandering-take-delay input)]
                  #:direction [direction (wandering-direction input)])
    (struct-copy wandering input
                 [info #:parent action new-info]
                 [take-delay take-delay]
                 [direction direction]))
 
  (let ([blocks (find-nearby-blocks (action-info input))])
    (if (and (= (wandering-take-delay input) 0) (> (length blocks) 0))
        (take-or-drop-block blocks)
        (try-to-move-bot))))

(module+ test
  (require rackunit "build-world.rkt" "location.rkt" "world.rkt")
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
   (build-world
    '(3 ("block" 1 0))
    (λ (world server ref)
      (let* ([action (make-wandering server (location 1 1) direction-east)]
             [new-action (wander server action)])
        (check-equal? (entity-location (action-bot new-action)) (location 1 1))
        (check-equal? (entity-cargo (action-bot new-action)) (ref "block"))))))

  (test-case
   "wandering delays taking"
   (build-world
    '(3 ("bot" 1 1) ("block" 1 0))
    (λ (world server ref)
      (let* ([action (wandering
                      (bot-info (world-size world) (ref "bot") (list (ref "block")))
                      wander direction-east 0 1)]
             [new-action (wander server action)])
        (check-equal? (entity-location (action-bot new-action)) (location 2 1) "bot moved")
        (check-equal? (wandering-take-delay new-action) 0 "delay decremented")
        (check-false (entity-cargo (action-bot new-action)) "not taken")))))
  
  (test-case
   "wandering drops nearby block"
   (build-world
    '(4 ("bot" 2 1) ("block1" 1 0) ("block2" 3 1))
    (λ (world server ref)
      (let* ([laden-bot (take-entity! world (entity-id (ref "bot")) (entity-id (ref "block1")))]
             [action (wandering
                      (bot-info (world-size world) laden-bot (list (ref "block2")))
                      wander direction-east 0 0)]
             [new-action (wander server action)])
        (check-equal? (wandering-take-delay new-action) 10 "delay started")
        (check-equal? (entity-location (entity-ref world (entity-id (ref "block1"))))
                      (location 2 2) "block dropped"))))))
