#lang racket

(provide (struct-out entity)
         entity-symbol make-entity
         direction-from-entity 
         type-block type-bot type-edge)
(require "direction.rkt" "location.rkt")

(struct entity (id type location cargo) #:prefab)

(define (make-entity id type location) (entity id type location #f))

(define type-bot 0)
(define type-block 1)
(define type-edge 2)

(define (entity-symbol entity)
  (let ([type (entity-type entity)])
    (cond
      [(= type type-bot) (if (entity-cargo entity) #\u25A3  #\u25A1)]
      [(= type type-block) #\u25A0]
      [else #\?])))

(define (direction-from-entity from to)
  (direction-from (entity-location from) (entity-location to)))

(module+ test
  (require rackunit)

  )