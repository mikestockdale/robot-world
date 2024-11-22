#lang racket

(provide blocks-nearby? find-removable-blocks direction-from 
         (struct-out update) direction-change-chance
         choose-drop choose-move choose-take)

(require "shared.rkt")
(module+ test (require rackunit))

;@title{Tactics}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/tactics.rkt" "tactics.rkt"]}
;This is a collection of functions for use inimplementing an action strategy. 

;The @bold{direction from} one location to another is a direction that will move a bot closer to a destination.

(test-case:
 "direction from location to location"
 (check-equal? (direction-from (location 1 1) (location 2 1)) direction-east)
 (check-equal? (direction-from (location 1 1) (location 3 4)) direction-north)
 (check-equal? (direction-from (location 1 1) (location 4 3)) direction-east)
 (check-equal? (direction-from (location 1 4) (location 3 1)) direction-south)
 (check-equal? (direction-from (location 4 1) (location 1 3)) direction-west))

;The direction returned will reduce the larger of the x and y difference.

(define (direction-from from to)
  (let-values ([(difference-x difference-y) (location-offset from to)])
    (if (> (abs difference-x) (abs difference-y))
        (if (positive? difference-x) direction-west direction-east)
        (if (positive? difference-y) direction-south direction-north))))

;------------------------------------------------------------------------------------

(struct update (type parameter direction delay))
(define direction-change-chance (make-parameter 0.2))

(define (choose-drop bot)
  (let ([drop-direction (best-drop-direction bot)])
    (update request-drop drop-direction
            (change-direction drop-direction) 5)))

(define (best-drop-direction bot)
  (define (score direction)
    (let ([location (move-direction direction (bot-location bot))])
      (if (is-free? location (bot-neighbors bot))
          (count-adjacent location bot)
          -1)))
  (for/fold ([best-direction 0]
             [best-score -1]
             #:result best-direction)
            ([direction all-directions])
    (let ([score (score direction)])
      (if (> score best-score)
          (values direction score)
          (values best-direction best-score)))))
  
(define (is-free? location neighbors)
  (not (findf (λ (neighbor) (equal? location (entity-location neighbor))) neighbors)))

(define (choose-move direction delay)
  (update request-move direction direction (max 0 (- delay 1))))

(define (choose-take bot block)
  (let ([take-direction (direction-from-entity (bot-entity bot) block)]) 
    (update request-take (entity-id block)
            take-direction 5)))

(define (direction-from-entity from to)
  (direction-from (entity-location from) (entity-location to)))

;------------------------------------------------------------------------------------

(define (entity-adjacent? a b)
  (entity-location-adjacent? a (entity-location b)))

(define (entity-location-adjacent? entity location)
  (adjacent? (entity-location entity) location))

(define (count-adjacent location bot)
  (count
   (λ (entity) (and
                (= (entity-type entity) type-block)
                (entity-location-adjacent? entity location)))
   (bot-neighbors bot)))

(define (find-removable-blocks bot)
  (filter (λ (entity)
            (and (= (entity-type entity) type-block)
                 (entity-adjacent? entity (bot-entity bot))
                 (< (count-adjacent (entity-location entity) bot) 2)))
          (bot-neighbors bot)))
  
(define (blocks-nearby? bot)
  (ormap
   (λ (entity) (= (entity-type entity) type-block))
   (bot-neighbors bot)))

 
(test-case:
 "adjacent entities"
 (check-true (entity-adjacent? (entity 101 type-bot (location 1 1))
                               (entity 102 type-bot (location 1 2))))
 (check-false (entity-adjacent? (entity 101 type-bot (location 1 1))
                                (entity 102 type-bot (location 2 2)))))  
  
(test-case:
 "removable block found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block1 (entity 102 type-block (location 1 2))]
        [block2 (entity 102 type-block (location 2 2))]
        [bot2 (entity 103 type-bot (location 2 1))]
        [bot (bot bot1 #f (list bot2 block1 block2))]
        [removable (find-removable-blocks bot)])
   (check-equal? (length removable) 1)
   (check-equal? (first removable) block1)))
  
(test-case:
 "block not removable"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block1 (entity 102 type-block (location 1 2))]
        [block2 (entity 102 type-block (location 2 2))]
        [block3 (entity 103 type-block (location 0 2))]
        [bot (bot bot1 #f (list block1 block2 block3))]
        [removable (find-removable-blocks bot)])
   (check-equal? (length removable) 0)))

    
(test-case:
 "free location found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block (entity 102 type-block (location 1 2))]
        [bot (bot bot1 #f (list block))])
   (check-equal? (best-drop-direction bot) direction-east)))

(test-case:
 "best location found"
 (let* ([bot1 (entity 101 type-bot (location 1 1))]
        [block1 (entity 102 type-block (location 0 0))]
        [block2 (entity 103 type-block (location 2 0))])
   (check-equal? (best-drop-direction (bot bot1 #f (list block1 block2)))
                 direction-south)))
    
(test-case:
 "free location not outside world"
 (let* ([bot1 (entity 101 type-bot (location 49 49))]
        [edge1 (make-edge (location 50 49))]
        [edge2 (make-edge (location 49 50))]
        [block (entity 102 type-block (location 49 48))]
        [bot (bot bot1 #f (list edge1 edge2 block))])
   (check-equal? (best-drop-direction bot) direction-west)))
  
(test-case:
 "blocks nearby"
 (check-true (blocks-nearby? (bot #f #f (list (entity 101 type-block #f)))))
 (check-false (blocks-nearby? (bot #f #f (list (entity 101 type-bot #f)))))
 (check-false (blocks-nearby? (bot #f #f '()))))
