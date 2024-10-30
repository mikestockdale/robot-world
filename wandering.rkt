#lang racket

(provide wandering-actions)

(require "action.rkt" "command.rkt" "shared.rkt" "strategy.rkt")

(struct wandering (direction direction-change-chance cargo-delay))

(define (wandering-actions bot-infos)
  (map
   (λ (bot-info) (action #f #f (wander (wandering direction-east 0.2 0)) #t bot-info))
   bot-infos))

(define ((wander spec) input-action)
  (let-values ([(command parameter new-spec) ((choose spec) input-action)])
    (struct-copy action input-action
                 [command command]
                 [parameter parameter]
                 [strategy (wander new-spec)])))

(define ((choose spec) input)
  
  (define (pick-direction)
    (let ([old-direction (wandering-direction spec)])
      (if (or (and (equal? (action-command input) move-command)
                   (not (action-success? input))) 
              (> (wandering-direction-change-chance spec) (random)))
          (change-direction old-direction)
          old-direction)))

  (define (choose-drop)
    (let ([drop-direction (best-drop-direction (action-info input))])
      (values drop-command drop-direction
              (struct-copy wandering spec
                           [direction (change-direction drop-direction)]
                           [cargo-delay 5]))))
  
  (define (choose-move)
    (let ([direction (pick-direction)])
      (values move-command direction
              (struct-copy wandering spec
                           [direction direction]
                           [cargo-delay (max 0 (- (wandering-cargo-delay spec) 1))]))))

  (define (choose-take block)
    (let ([take-direction (direction-from-entity (action-bot input) block)]) 
      (values take-command (entity-id block)
              (struct-copy wandering spec
                           [direction take-direction]
                           [cargo-delay 5]))))
  
  (if (and (= (wandering-cargo-delay spec) 0)
           (entity-cargo (action-bot input))
           (blocks-nearby? (action-info input)))
      (choose-drop)
      (let ([blocks (find-removable-blocks (action-info input))])
        (if (and (= (wandering-cargo-delay spec) 0)
                 (> (length blocks) 0))
            (choose-take (first blocks))
            (choose-move)))))

(module+ test
  (require rackunit "world.rkt")

  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action command #f choose success
            (bot-info (entity 101 type-bot (location 1 1) cargo) neighbors)))
  
  (define (wander-with
           #:chance [chance 0]
           #:cargo-delay [cargo-delay 0])
    (choose (wandering direction-east chance cargo-delay)))

  (test-case
   "move in current direction"
   (let-values ([(command parameter spec) ((wander-with #:cargo-delay 5) (choose-input))])
     (check-equal? command move-command)
     (check-equal? parameter direction-east)
     (check-equal? (wandering-cargo-delay spec) 4)))
  
  (test-case
   "move in random direction"
   (let-values ([(command parameter spec)
                 ((wander-with #:chance 1) (choose-input))])
     (check-equal? command move-command)
     (check-not-equal? parameter direction-east)))
  
  (test-case
   "change direction if can't move"
   (let-values ([(command parameter spec)
                 ((wander-with) (choose-input #:success #f #:command move-command))])
     (check-equal? command move-command)
     (check-not-equal? parameter direction-east)))
  
  (test-case
   "take nearby block"
   (let-values ([(command parameter spec)
                 ((wander-with)
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0) #f))))])
     (check-equal? command take-command)
     (check-equal? parameter 102)
     (check-equal? (wandering-direction spec) direction-south)
     (check-equal? (wandering-cargo-delay spec) 5)))
  
  (test-case
   "delay taking nearby block"
   (let-values ([(command parameter procedure)
                 ((wander-with #:cargo-delay 1)
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0) #f))))])
     (check-equal? command move-command)))

  (test-case
   "drop nearby block"
   (let-values ([(command parameter spec)
                 ((wander-with)
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2) #f))
                                #:cargo (entity 103 type-block (location 0 0) #f)))])
     (check-equal? command drop-command)
     (check-equal? parameter direction-north)
     (check-equal? (wandering-cargo-delay spec) 5)
     (check-not-equal? (wandering-direction spec) direction-north)))

  (test-case
   "delay dropping nearby block"
   (let-values ([(command parameter spec)
                 ((wander-with #:cargo-delay 1)
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2) #f))
                                #:cargo (entity 103 type-block (location 0 0) #f)))])
     (check-equal? command move-command))))
