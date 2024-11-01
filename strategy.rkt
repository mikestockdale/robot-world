#lang racket

(provide best-drop-direction blocks-nearby? find-removable-blocks
         direction-from direction-from-entity)

(require "shared.rkt")

(define (entity-adjacent? a b)
  (entity-location-adjacent? a (entity-location b)))

(define (entity-location-adjacent? entity location)
  (adjacent? (entity-location entity) location))

(define (direction-from from to)
  (let-values ([(difference-x difference-y) (location-offset from to)])
    (if (> (abs difference-x) (abs difference-y))
        (if (positive? difference-x) direction-west direction-east)
        (if (positive? difference-y) direction-south direction-north))))

(define (direction-from-entity from to)
  (direction-from (entity-location from) (entity-location to)))

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
  
(define (is-free? location neighbors)
  (not (findf (λ (neighbor) (equal? location (entity-location neighbor))) neighbors)))

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
  
(define (blocks-nearby? bot)
  (ormap
   (λ (entity) (= (entity-type entity) type-block))
   (bot-neighbors bot)))

(module+ test
  (require rackunit)
  
  (test-case
   "adjacent entities"
   (check-true (entity-adjacent? (entity 101 type-bot (location 1 1))
                                 (entity 102 type-bot (location 1 2))))
   (check-false (entity-adjacent? (entity 101 type-bot (location 1 1))
                                  (entity 102 type-bot (location 2 2)))))  
  (test-case
   "direction from location to location"
   (check-equal? (direction-from (location 1 1) (location 2 1)) direction-east)
   (check-equal? (direction-from (location 1 1) (location 3 4)) direction-north)
   (check-equal? (direction-from (location 1 1) (location 4 3)) direction-east)
   (check-equal? (direction-from (location 1 4) (location 3 1)) direction-south)
   (check-equal? (direction-from (location 4 1) (location 1 3)) direction-west))
  
  (test-case
   "removable block found"
   (let* ([bot1 (entity 101 type-bot (location 1 1))]
          [block1 (entity 102 type-block (location 1 2))]
          [block2 (entity 102 type-block (location 2 2))]
          [bot2 (entity 103 type-bot (location 2 1))]
          [bot (bot bot1 #f (list bot2 block1 block2))]
          [removable (find-removable-blocks bot)])
     (check-equal? (length removable) 1)
     (check-equal? (first removable) block1)))
  
  (test-case
   "block not removable"
   (let* ([bot1 (entity 101 type-bot (location 1 1))]
          [block1 (entity 102 type-block (location 1 2))]
          [block2 (entity 102 type-block (location 2 2))]
          [block3 (entity 103 type-block (location 0 2))]
          [bot (bot bot1 #f (list block1 block2 block3))]
          [removable (find-removable-blocks bot)])
     (check-equal? (length removable) 0)))

    
  (test-case
   "free location found"
   (let* ([bot1 (entity 101 type-bot (location 1 1))]
          [block (entity 102 type-block (location 1 2))]
          [bot (bot bot1 #f (list block))])
     (check-equal? (best-drop-direction bot) direction-east)))

  (test-case
   "best location found"
   (let* ([bot1 (entity 101 type-bot (location 1 1))]
          [block1 (entity 102 type-block (location 0 0))]
          [block2 (entity 103 type-block (location 2 0))])
     (check-equal? (best-drop-direction (bot bot1 #f (list block1 block2)))
                   direction-south)))
    
  (test-case
   "free location not outside world"
   (let* ([bot1 (entity 101 type-bot (location 49 49))]
          [edge1 (make-edge (location 50 49))]
          [edge2 (make-edge (location 49 50))]
          [block (entity 102 type-block (location 49 48))]
          [bot (bot bot1 #f (list edge1 edge2 block))])
     (check-equal? (best-drop-direction bot) direction-west)))
  
  (test-case
   "blocks nearby"
   (check-true (blocks-nearby? (bot #f #f (list (entity 101 type-block #f)))))
   (check-false (blocks-nearby? (bot #f #f (list (entity 101 type-bot #f)))))
   (check-false (blocks-nearby? (bot #f #f '())))))