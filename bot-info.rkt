#lang racket

(provide (struct-out bot-info)
         best-drop-direction find-adjacent-blocks blocks-nearby?
         bot-info->list list->bot-info)

(require "direction.rkt" "entity.rkt" "location.rkt")

(struct bot-info (bot neighbors) #:transparent)

(define (find-adjacent-blocks info)
  (filter (λ (entity)
            (and (= (entity-type entity) type-block)
                 (= (entity-distance entity (bot-info-bot info)) 1)))
          (bot-info-neighbors info)))

(define (blocks-nearby? info)
  (ormap
   (λ (entity) (= (entity-type entity) type-block))
   (bot-info-neighbors info)))

(define (is-free? location neighbors)
  (not (findf (λ (neighbor) (equal? location (entity-location neighbor))) neighbors)))

(define (best-drop-direction info)

  (define (score direction)
    (let ([location (move-direction direction (entity-location (bot-info-bot info)))])

      (define (count-neighbor hand-of)
        (if (is-free? (move-direction (hand-of direction) location)
                      (bot-info-neighbors info))
            0 1))
      
      (if (is-free? location (bot-info-neighbors info))
          (+ (count-neighbor left-of) (count-neighbor right-of))
          -1)))
  
  (foldl (λ (a b) (if (> (score a) (score b)) a b))
         (first all-directions) (rest all-directions))) 

(define (bot-info->list info)
  (list (entity->list (bot-info-bot info))
        (map entity->list (bot-info-neighbors info))))

(define (list->bot-info list)
  (bot-info (list->entity (first list))
            (map list->entity (second list))))

(module+ test
  (require rackunit "location.rkt")
  
  (test-case
   "adjacent block found"
   (let* ([bot1 (make-entity 101 type-bot (location 1 1))]
          [block1 (make-entity 102 type-block (location 1 2))]
          [block2 (make-entity 102 type-block (location 2 2))]
          [bot2 (make-entity 103 type-bot (location 2 1))]
          [info (bot-info bot1 (list bot2 block1 block2))]
          [adjacent (find-adjacent-blocks info)])
     (check-equal? (length adjacent) 1)
     (check-equal? (first adjacent) block1)))

  (test-case
   "blocks nearby"
   (check-true (blocks-nearby? (bot-info #f (list (make-entity 101 type-block #f)))))
   (check-false (blocks-nearby? (bot-info #f (list (make-entity 101 type-bot #f)))))
   (check-false (blocks-nearby? (bot-info #f '()))))
  
  (test-case
   "free location found"
   (let* ([bot (make-entity 101 type-bot (location 1 1))]
          [block (make-entity 102 type-block (location 1 2))]
          [info (bot-info bot (list block))])
     (check-equal? (best-drop-direction info) direction-east)))

  (test-case
   "best location found"
   (let* ([bot (make-entity 101 type-bot (location 1 1))]
          [block1 (make-entity 102 type-block (location 0 0))]
          [block2 (make-entity 103 type-block (location 2 0))])
     (check-equal? (best-drop-direction (bot-info bot (list block1 block2)))
                   direction-south)))
  
  (test-case
   "free location not outside world"
   (let* ([bot (make-entity 101 type-bot (location 49 49))]
          [edge1 (make-entity 0 type-edge (location 50 49))]
          [edge2 (make-entity 0 type-edge (location 49 50))]
          [block (make-entity 102 type-block (location 49 48))]
          [info (bot-info bot (list edge1 edge2 block))])
     (check-equal? (best-drop-direction info) direction-west)))

  (test-case
   "convert to and from list"
   (let ([info (bot-info (entity 1 2 (location 3 4) #f)
                         (list
                          (entity 6 7 (location 8 9) #f)
                          (entity 11 12 (location 13 14) #f)))])
     (check-equal? (list->bot-info (bot-info->list info)) info))))
