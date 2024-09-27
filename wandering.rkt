#lang racket

(provide make-wandering)

(require "actions.rkt" "bot-info.rkt" "direction.rkt" "execute.rkt" "entity.rkt" "server.rkt")

(struct wandering (direction direction-change-chance take-delay))

(define (make-wandering server location direction [chance 0.2])
  (action #f #f (wander (wandering direction chance 0)) (add-bot! server location)))

(define ((wander spec) input)
  (let-values ([(execute parameter new-spec) ((choose spec) input)])
    (values execute parameter (wander new-spec))))

(define ((choose spec) input)
  
  (define (pick-direction)
    (let ([old-direction (wandering-direction spec)])
      (if (or (and (equal? (action-execute input) execute-move)
                   (not (bot-info-success? (action-info input)))) 
              (> (wandering-direction-change-chance spec) (random)))
          (change-direction old-direction)
          old-direction)))

  (define (choose-drop)
    (let ([drop-direction (find-free-direction (action-info input))])
      (values execute-drop drop-direction
              (struct-copy wandering spec
                           [direction (change-direction drop-direction)]
                           [take-delay 10]))))
  
  (define (choose-move)
    (let ([direction (pick-direction)])
      (values execute-move direction
              (struct-copy wandering spec
                           [direction direction]
                           [take-delay (max 0 (- (wandering-take-delay spec) 1))]))))

  (define (choose-take block)
    (let ([take-direction (direction-from-entity (action-bot input) block)]) 
      (values execute-take (entity-id block)
              (struct-copy wandering spec
                           [direction take-direction])))) 
  
  (let ([blocks (find-nearby-blocks (action-info input))])
    (if (and (= (wandering-take-delay spec) 0) (> (length blocks) 0))
        (if (entity-cargo (action-bot input))
            (choose-drop)
            (choose-take (first blocks)))
        (choose-move))))


(module+ test
  (require rackunit "location.rkt" "world.rkt")

  (define (choose-input
           #:success [success #t]
           #:cargo [cargo #f]
           #:execute [execute #f]
           #:neighbors [neighbors '()])
    (action execute #f choose
            (bot-info 3 success (entity 101 type-bot (location 1 1) cargo) neighbors)))
  
  (define (wander-with
           #:chance [chance 0]
           #:take-delay [take-delay 0])
    (choose (wandering direction-east chance take-delay)))

  (test-case
   "move in current direction"
   (let-values ([(execute parameter spec) ((wander-with #:take-delay 5) (choose-input))])
     (check-equal? execute execute-move)
     (check-equal? parameter direction-east)
     (check-equal? (wandering-take-delay spec) 4)))
  
  (test-case
   "move in random direction"
   (let-values ([(execute parameter spec)
                 ((wander-with #:chance 1) (choose-input))])
     (check-equal? execute execute-move)
     (check-not-equal? parameter direction-east)))
  
  (test-case
   "change direction if can't move"
   (let-values ([(execute parameter spec)
                 ((wander-with) (choose-input #:success #f #:execute execute-move))])
     (check-equal? execute execute-move)
     (check-not-equal? parameter direction-east)))
  
  (test-case
   "take nearby block"
   (let-values ([(execute parameter spec)
                 ((wander-with)
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0) #f))))])
     (check-equal? execute execute-take)
     (check-equal? parameter 102)
     (check-equal? (wandering-direction spec) direction-south)))
  
  (test-case
   "delay taking nearby block"
   (let-values ([(execute parameter procedure)
                 ((wander-with #:take-delay 1)
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0) #f))))])
     (check-equal? execute execute-move)))

  (test-case
   "drop nearby block"
   (let-values ([(execute parameter spec)
                 ((wander-with)
                  (choose-input #:neighbors (list (entity 102 type-block (location 1 0) #f))
                                #:cargo (entity 103 type-block (location 0 0) #f)))])
     (check-equal? execute execute-drop)
     (check-equal? parameter direction-north)
     (check-equal? (wandering-take-delay spec) 10)
     (check-not-equal? (wandering-direction spec) direction-north))))
