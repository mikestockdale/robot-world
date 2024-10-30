#lang racket

(require "shared.rkt" "viewer.rkt")

(define (timer) #f)

(define (adjacent draw-entity)
  (draw-entity (entity-symbol (make-entity #f type-bot #f)) 2 2)
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 1 2 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 3 2 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 2 1 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 2 3 #:color "light gray"))

(define (nearby draw-entity)
  (draw-entity (entity-symbol (make-entity #f type-bot #f)) 2 2)
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 1 1 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 1 2 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 1 3 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 2 1 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 2 3 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 3 1 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 3 2 #:color "light gray")
  (draw-entity (entity-symbol (make-entity #f type-block #f)) 3 3 #:color "light gray"))

(run-viewer "test" nearby timer #:size 5 #:style '(no-caption))
