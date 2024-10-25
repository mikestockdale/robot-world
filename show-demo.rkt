#lang racket

(require "entity.rkt" "viewer.rkt")

(define (timer) #f)

(define (draw draw-entity)
  (draw-entity (entity-symbol (make-entity #f type-bot #f)) 2 2)
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 1 2 #:color "green")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 3 2 #:color "green")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 2 1 #:color "green")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 2 3 #:color "green"))

(run-viewer "test" draw timer #:size 5 #:style '(no-caption))
