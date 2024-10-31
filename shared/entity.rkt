#lang racket

(provide (struct-out entity) make-edge
         entity-symbol change-entity-location
         type-block type-bot type-edge)

(struct entity (id type location) #:prefab)

(define type-bot 0)
(define type-block 1)
(define type-edge 2)

(define (make-edge location) (entity 0 type-edge location))

(define (change-entity-location source new-location)
  (struct-copy entity source [location new-location]))

(define (entity-symbol entity [laden? #f])
  (let ([type (entity-type entity)])
    (cond
      [(= type type-bot) (if laden? #\u25A3  #\u25A1)]
      [(= type type-block) #\u25A0]
      [else #\?])))

(module+ test
  (require rackunit "location.rkt")

  (test-case
   "change entity location"
   (check-equal?
    (change-entity-location (entity 101 type-bot (location 1 2)) (location 3 4))
    (entity 101 type-bot (location 3 4))))

  (test-case
   "entity symbols"
   (check-equal? (entity-symbol (entity 0 type-bot #f)) #\u25A1)
   (check-equal? (entity-symbol (entity 0 type-bot #f) 'laden) #\u25A3)
   (check-equal? (entity-symbol (entity 0 type-block #f)) #\u25A0)
   (check-equal? (entity-symbol (entity 0 type-edge #f)) #\?)))
