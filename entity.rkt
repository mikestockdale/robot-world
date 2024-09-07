#lang racket

(provide (struct-out entity)
         entity-symbol
         type-block type-bot)

(struct entity (id type location))

(define type-bot 0)
(define type-block 1)
(define type-symbols #(#\O #\B))

(define (entity-symbol entity) (vector-ref type-symbols (entity-type entity)))
