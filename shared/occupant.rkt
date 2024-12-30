#lang racket/base

(provide (struct-out occupantx) occupantx-id occupantx-type)
(require "entity.rkt")

;@title{Occupant}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/occupant.rkt" "occupant.rkt"]}
;An occupant is an entity at a location.

(struct occupantx (entity location) #:prefab)
(define (occupantx-id occupant) (entity-id (occupantx-entity occupant)))
(define (occupantx-type occupant) (entity-type (occupantx-entity occupant)))
