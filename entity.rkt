#lang racket

(provide (struct-out entity)
         entity-symbol make-entity
         type-block type-bot)

(struct entity (id type location cargo))

(define (make-entity id type location) (entity id type location #f))

(define type-bot 0)
(define type-block 2)
(define type-symbols #(#\u25A1 #\u25A3 #\u25A0))

(define (entity-symbol entity)
  (vector-ref type-symbols
              (if (entity-cargo entity)
                  (add1 (entity-type entity))
                  (entity-type entity))))
