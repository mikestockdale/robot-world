#lang racket

(provide wandering-actions)

(require "client/action.rkt" "shared.rkt" "client/tactics.rkt")

(struct wandering (direction cargo-delay))

(define (wandering-actions replies)
  (map
   (λ (reply) (action (wander (wandering direction-east 0)) #f #f #t (reply-bot reply)))
   replies))

(define ((wander spec) input-action)
  (let ([choice (choose spec input-action)])
    (struct-copy
     action input-action
     [request-type (choice-type choice)]
     [parameter (choice-parameter choice)]
     [strategy (wander (struct-copy
                        wandering spec
                        [direction (choice-direction choice)]
                        [cargo-delay (choice-delay choice)]))])))

(define (choose spec input)
  
  (define (pick-direction)
    (let ([old-direction (wandering-direction spec)])
      (if (or (and (equal? (action-request-type input) request-move)
                   (not (action-success? input))) 
              (> (direction-change-chance) (random)))
          (change-direction old-direction)
          old-direction)))

  (if (and (= (wandering-cargo-delay spec) 0)
           (bot-cargo (action-bot input))
           (blocks-nearby? (action-bot input)))
      (choose-drop (action-bot input))
      (let ([blocks (removable-blocks (action-bot input))])
        (if (and (= (wandering-cargo-delay spec) 0)
                 (> (length blocks) 0))
            (choose-take (action-bot input) (first blocks))
            (choose-move (pick-direction) (wandering-cargo-delay spec))))))

(module+ test
  (require rackunit)

  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action choose command #f success
            (bot (entity 101 type-bot (location 1 1)) cargo neighbors)))
  
  (define (wander-with
           #:chance [chance 0]
           #:cargo-delay [cargo-delay 0]
           input)
    (parameterize ([direction-change-chance chance])
      (choose (wandering direction-east cargo-delay) input)))

  (test-case
   "move in current direction"
   (let ([choice (wander-with #:cargo-delay 5 (choose-input))])
     (check-equal? (choice-type choice) request-move)
     (check-equal? (choice-parameter choice) direction-east)
     (check-equal? (choice-delay choice) 4)))
  
  (test-case
   "move in random direction"
   (let ([choice (wander-with #:chance 1 (choose-input))])
     (check-equal? (choice-type choice) request-move)
     (check-not-equal? (choice-parameter choice) direction-east)))
  
  (test-case
   "change direction if can't move"
   (let ([choice (wander-with (choose-input #:success #f #:command request-move))])
     (check-equal? (choice-type choice) request-move)
     (check-not-equal? (choice-parameter choice) direction-east)))
  
  (test-case
   "take nearby block"
   (let ([choice (wander-with
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
     (check-equal? (choice-type choice) request-take)
     (check-equal? (choice-parameter choice) 102)
     (check-equal? (choice-direction choice) direction-south)
     (check-equal? (choice-delay choice) 5)))
  
  (test-case
   "delay taking nearby block"
   (let ([choice (wander-with
                  #:cargo-delay 1
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
     (check-equal? (choice-type choice) request-move)))

  (test-case
   "drop nearby block"
   (let ([choice (wander-with
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                #:cargo (entity 103 type-block (location 0 0))))])
     (check-equal? (choice-type choice) request-drop)
     (check-equal? (choice-parameter choice) direction-north)
     (check-equal? (choice-delay choice) 5)
     (check-not-equal? (choice-direction choice) direction-north)))

  (test-case
   "delay dropping nearby block"
   (let ([choice (wander-with
                  #:cargo-delay 1
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                #:cargo (entity 103 type-block (location 0 0))))])
     (check-equal? (choice-type choice) request-move))))
