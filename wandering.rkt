#lang racket

(provide wandering-actions)

(require "client/action.rkt" "shared.rkt" "client/tactics.rkt")

(struct wandering (direction cargo-delay))

(define (wandering-actions bot-infos)
  (map
   (λ (bot-info) (action (wander (wandering direction-east 0)) #f #f #t bot-info))
   bot-infos))

(define ((wander spec) input-action)
  (let ([update (choose spec input-action)])
    (struct-copy
     action input-action
     [request-type (update-type update)]
     [parameter (update-parameter update)]
     [strategy (wander (struct-copy
                        wandering spec
                        [direction (update-direction update)]
                        [cargo-delay (update-delay update)]))])))

(define (choose spec input)
  
  (define (pick-direction)
    (let ([old-direction (wandering-direction spec)])
      (if (or (and (equal? (action-request-type input) request-move)
                   (not (action-success? input))) 
              (> (direction-change-chance) (random)))
          (change-direction old-direction)
          old-direction)))

  (define (choose-drop)
    (let ([drop-direction (best-drop-direction (action-bot input))])
      (update request-drop drop-direction
              (change-direction drop-direction) 5)))
  
  (define (choose-move)
    (let ([direction (pick-direction)])
      (update request-move direction
              direction (max 0 (- (wandering-cargo-delay spec) 1)))))

  (define (choose-take block)
    (let ([take-direction (direction-from-entity (bot-entity (action-bot input)) block)]) 
      (update request-take (entity-id block) take-direction 5)))
  
  (if (and (= (wandering-cargo-delay spec) 0)
           (bot-cargo (action-bot input))
           (blocks-nearby? (action-bot input)))
      (choose-drop)
      (let ([blocks (find-removable-blocks (action-bot input))])
        (if (and (= (wandering-cargo-delay spec) 0)
                 (> (length blocks) 0))
            (choose-take (first blocks))
            (choose-move)))))

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
   (let ([update (wander-with #:cargo-delay 5 (choose-input))])
     (check-equal? (update-type update) request-move)
     (check-equal? (update-parameter update) direction-east)
     (check-equal? (update-delay update) 4)))
  
  (test-case
   "move in random direction"
   (let ([update (wander-with #:chance 1 (choose-input))])
     (check-equal? (update-type update) request-move)
     (check-not-equal? (update-parameter update) direction-east)))
  
  (test-case
   "change direction if can't move"
   (let ([update (wander-with (choose-input #:success #f #:command request-move))])
     (check-equal? (update-type update) request-move)
     (check-not-equal? (update-parameter update) direction-east)))
  
  (test-case
   "take nearby block"
   (let ([update (wander-with
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
     (check-equal? (update-type update) request-take)
     (check-equal? (update-parameter update) 102)
     (check-equal? (update-direction update) direction-south)
     (check-equal? (update-delay update) 5)))
  
  (test-case
   "delay taking nearby block"
   (let ([update (wander-with
                  #:cargo-delay 1
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
     (check-equal? (update-type update) request-move)))

  (test-case
   "drop nearby block"
   (let ([update (wander-with
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                #:cargo (entity 103 type-block (location 0 0))))])
     (check-equal? (update-type update) request-drop)
     (check-equal? (update-parameter update) direction-north)
     (check-equal? (update-delay update) 5)
     (check-not-equal? (update-direction update) direction-north)))

  (test-case
   "delay dropping nearby block"
   (let ([update (wander-with
                  #:cargo-delay 1
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                #:cargo (entity 103 type-block (location 0 0))))])
     (check-equal? (update-type update) request-move))))
