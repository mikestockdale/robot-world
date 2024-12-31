#lang racket/base

(provide (struct-out occupant) occupant-id occupant-type)
(require "entity.rkt")

;@title{Occupant}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/occupant.rkt" "occupant.rkt"]}
;An occupant is an entity at a location.

(struct occupant (entity location) #:prefab)
(define (occupant-id occupant) (entity-id (occupant-entity occupant)))
(define (occupant-type occupant) (entity-type (occupant-entity occupant)))
