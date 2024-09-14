#lang racket

(provide (struct-out entity)
         entity-symbol make-entity entity->list list->entity
         type-block type-bot)
(require "location.rkt")

(struct entity (id type location cargo)  #:transparent)

(define (make-entity id type location) (entity id type location #f))

(define type-bot 0)
(define type-block 2)
(define type-symbols #(#\u25A1 #\u25A3 #\u25A0))

(define (entity-symbol entity)
  (vector-ref type-symbols
              (if (entity-cargo entity)
                  (add1 (entity-type entity))
                  (entity-type entity))))

(define (entity->list entity)
  (list (entity-id entity)
        (entity-type entity)
        (location->list (entity-location entity))
        (if (entity-cargo entity) (entity->list (entity-cargo entity)) #f)))

(define (list->entity list)
  (entity (first list)
          (second list)
          (apply location (third list))
          (if (fourth list) (list->entity (fourth list)) #f)))

(module+ test
  (require rackunit)

  (test-case
   "convert to and from list"
   (let ([entity (entity 1 2 (location 3 4) #f)])
   (check-equal? (list->entity (entity->list entity)) entity)))

  (test-case
   "convert with cargo"
   (let ([entity (entity 1 2 (location 3 4) (entity 5 6 (location 7 8) #f))])
   (check-equal? (list->entity (entity->list entity)) entity))))