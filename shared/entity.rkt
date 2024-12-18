#lang racket/base

(provide (struct-out entity) make-edge
         type-block type-bot type-edge type-base)

;@title{Entity}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/entity.rkt" "entity.rkt"]}
;The game world is populated with entities.
;An entity has a unique identifier, and a type.

(struct entity (id type) #:prefab)

;Bots are controlled by game clients, and can move around the world.
;Bases are places where items can be delivered.
;Blocks are passive, and can be picked up, carried, and dropped by bots.
;Edges are a special type, generated by the server, to show bots where the edge of the world is.

(define type-bot 0)
(define type-base 1)
(define type-block 2)
(define type-edge 3)

(define (make-edge) (entity 0 type-edge))
