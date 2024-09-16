#lang racket

(provide (struct-out bot-info) find-free-direction find-nearby-blocks
         bot-info->list list->bot-info)

(require "direction.rkt" "entity.rkt")

(struct bot-info (bot neighbors) #:transparent)

(define (find-nearby-blocks info)
  (filter (λ (entity) (= (entity-type entity) type-block))
          (bot-info-neighbors info)))

(define (find-free-direction info)
  (find-direction
   (λ (direction)
     (is-free? (move-direction direction (entity-location (bot-info-bot info)))
               (bot-info-neighbors info)))))

(define (is-free? location neighbors)
  (not (findf (λ (neighbor) (equal? location (entity-location neighbor))) neighbors)))

(define (bot-info->list info)
  (list (entity->list (bot-info-bot info))
        (map entity->list (bot-info-neighbors info))))

(define (list->bot-info list)
  (bot-info (list->entity (first list))
            (map list->entity (second list))))

(module+ test
  (require rackunit "location.rkt")
  
  (test-case
   "nearby block found"
   (let* ([bot1 (make-entity 101 type-bot (location 1 1))]
          [block (make-entity 102 type-block (location 1 2))]
          [bot2 (make-entity 103 type-bot (location 2 1))]
          [info (bot-info bot1 (list bot2 block))]
          [nearby (find-nearby-blocks info)])
     (check-equal? (length nearby) 1)
     (check-equal? (first nearby) block)))
  
  (test-case
   "free location found"
   (let* ([bot (make-entity 101 type-bot (location 1 1))]
          [block (make-entity 102 type-block (location 1 2))]
          [info (bot-info bot (list block))])
     (check-equal? (find-free-direction info) direction-east)))

  (test-case
   "convert to and from list"
   (let ([info (bot-info (entity 1 2 (location 3 4) #f)
                         (list
                          (entity 6 7 (location 8 9) #f)
                          (entity 11 12 (location 13 14) #f)))])
     (check-equal? (list->bot-info (bot-info->list info)) info))))
