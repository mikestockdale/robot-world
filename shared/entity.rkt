#lang racket

(provide (struct-out entity)
         entity-symbol make-entity relocate-entity load-entity
         type-block type-bot type-edge)

(struct entity (id type location cargo) #:prefab)

(define type-bot 0)
(define type-block 1)
(define type-edge 2)

(define (make-entity id type location) (entity id type location #f))

(define (relocate-entity source new-location)
  (struct-copy entity source [location new-location]))

(define (load-entity source new-cargo)
  (struct-copy entity source [cargo new-cargo]))

(define (entity-symbol entity)
  (let ([type (entity-type entity)])
    (cond
      [(= type type-bot) (if (entity-cargo entity) #\u25A3  #\u25A1)]
      [(= type type-block) #\u25A0]
      [else #\?])))

(module+ test
  (require rackunit "location.rkt")

  (test-case
   "change entity location"
   (check-equal?
    (relocate-entity (make-entity 101 type-bot (location 1 2)) (location 3 4))
    (make-entity 101 type-bot (location 3 4))))

  (test-case
   "change entity cargo"
   (let ([cargo (make-entity 102 type-block (location 3 4))])
   (check-equal?
    (load-entity (make-entity 101 type-bot (location 1 2)) cargo)
    (entity 101 type-bot (location 1 2) cargo))))

  (test-case
   "entity symbols"
   (check-equal? (entity-symbol (entity 0 type-bot #f #f)) #\u25A1)
   (check-equal? (entity-symbol (entity 0 type-bot #f #t)) #\u25A3)
   (check-equal? (entity-symbol (entity 0 type-block #f #f)) #\u25A0)
   (check-equal? (entity-symbol (entity 0 type-edge #f #f)) #\?)))
