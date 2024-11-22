#lang racket

(provide gathering-actions)

(require "shared.rkt" "client/action.rkt" "client/tactics.rkt")

(struct gathering (direction cargo-delay location))

(define (gathering-actions replies)
  (map
   (λ (reply)
     (action (gather (gathering direction-east 0 (location 25 25))) #f #f #t (second reply)))
   replies))

(define ((gather spec) input-action)
  (let ([update (choose spec input-action)])
    (struct-copy
     action input-action
     [request-type (update-type update)]
     [parameter (update-parameter update)]
     [strategy (gather (struct-copy
                        gathering spec
                        [direction (update-direction update)]
                        [cargo-delay (update-delay update)]))])))

(define (choose spec input)

  (define (pick-direction)
    (if (bot-cargo (action-bot input))
        (direction-from
         (bot-location (action-bot input)) (gathering-location spec))
        (let ([old-direction (gathering-direction spec)])
          (if (or (and (equal? (action-request-type input) request-move)
                       (not (action-success? input))) 
                  (> (direction-change-chance) (random)))
              (change-direction old-direction)
              old-direction))))
  
  (if (and (= (gathering-cargo-delay spec) 0)
           (bot-cargo (action-bot input))
           (or
            (blocks-nearby? (action-bot input))
            (equal? (gathering-location spec) (bot-location (action-bot input)))))
      (choose-drop (action-bot input))
      (let ([blocks (find-removable-blocks (action-bot input))])
        (if (and (= (gathering-cargo-delay spec) 0)
                 (> (length blocks) 0))
            (choose-take (action-bot input) (first blocks))
            (choose-move (pick-direction) (gathering-cargo-delay spec))))))

(module+ test
  (require rackunit)

  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:command [command #f]
           #:neighbors [neighbors '()])
    (action #f command #f success
            (bot (entity 101 type-bot (location 1 1)) cargo neighbors)))
  
  (define (gather-with
           #:chance [chance 0]
           #:cargo-delay [cargo-delay 0]
           input)
    (parameterize ([direction-change-chance chance])
      (choose (gathering direction-east cargo-delay (location 1 1)) input)))

  (test-case
   "move in current direction"
   (let ([update (gather-with #:cargo-delay 5 (choose-input))])
     (check-equal? (update-type update) request-move)
     (check-equal? (update-parameter update) direction-east)
     (check-equal? (update-delay update) 4)))
  
  (test-case
   "move in random direction"
   (let ([update (gather-with #:chance 1 (choose-input))])
     (check-equal? (update-type update) request-move)
     (check-not-equal? (update-parameter update) direction-east)))
  
  (test-case
   "change direction if can't move"
   (let ([update (gather-with (choose-input #:success #f #:command request-move))])
     (check-equal? (update-type update) request-move)
     (check-not-equal? (update-parameter update) direction-east)))
  
  (test-case
   "take nearby block"
   (let ([update (gather-with
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
     (check-equal? (update-type update) request-take)
     (check-equal? (update-parameter update) 102)
     (check-equal? (update-direction update) direction-south)
     (check-equal? (update-delay update) 5)))
  
  (test-case
   "delay taking nearby block"
   (let ([update (gather-with #:cargo-delay 1
                              (choose-input #:neighbors (list (entity 102 type-block (location 1 0)))))])
     (check-equal? (update-type update) request-move)))

  (test-case
   "drop nearby block"
   (let ([update (gather-with
                  (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                #:cargo (entity 103 type-block (location 0 0))))])
     (check-equal? (update-type update) request-drop)
     (check-equal? (update-parameter update) direction-north)
     (check-equal? (update-delay update) 5)
     (check-not-equal? (update-direction update) direction-north)))

  (test-case
   "delay dropping nearby block"
   (let ([update (gather-with #:cargo-delay 1
                              (choose-input #:neighbors (list (entity 102 type-block (location 2 2)))
                                            #:cargo (entity 103 type-block (location 0 0))))])
     (check-equal? (update-type update) request-move)))

  ;drop at target location
  )
