#lang racket

(require "shared.rkt" "viewer.rkt")

(define (timer) (sleep .1))

(define (adjacent draw-entity)
  (draw-entity (entity-symbol (entity #f type-bot #f)) 2 2)
  (draw-entity (entity-symbol (entity #f type-block #f)) 1 2 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 3 2 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 2 1 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 2 3 #:color "light gray"))

(define (nearby draw-entity)
  (draw-entity (entity-symbol (entity #f type-bot #f)) 2 2)
  (draw-entity (entity-symbol (entity #f type-block #f)) 1 1 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 1 2 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 1 3 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 2 1 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 2 3 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 3 1 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 3 2 #:color "light gray")
  (draw-entity (entity-symbol (entity #f type-block #f)) 3 3 #:color "light gray"))

(define (bottom draw-entity)
  (draw-entity (entity-symbol (entity #f type-bot #f)) 0 49)
  (draw-entity (entity-symbol (entity #f type-bot #f)) 49 49))

;(run-viewer "test" nearby timer #:size 5 #:style '(no-caption))
(run-viewer "test" bottom timer)
