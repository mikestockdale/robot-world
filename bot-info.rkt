#lang racket

(provide (struct-out bot-info) find-free-direction)

(require "direction.rkt" "entity.rkt")
(module+ test (require rackunit "location.rkt"))

(struct bot-info (bot neighbors))

(define (find-free-direction info)
  (find-direction
   (λ (direction)
     (is-free? (move-direction direction (entity-location (bot-info-bot info)))
               (bot-info-neighbors info)))))

(define (is-free? location neighbors)
  (not (findf (λ (neighbor) (equal? location (entity-location neighbor))) neighbors)))

(module+ test
  (test-case
   "free location found"
   (let* ([bot (make-entity 101 type-bot (location 1 1))]
          [block (make-entity 102 type-block (location 1 2))]
          [info (bot-info bot (list block))])
     (check-equal? (find-free-direction info) direction-east))))

