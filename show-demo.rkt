#lang racket

(require "shared.rkt" "client/viewer.rkt")

(define (timer) (sleep .1))

(define (adjacent draw-entity)
  (draw-entity type-bot #f 2 2)
  (draw-entity type-block #f 1 2 #:color "light gray")
  (draw-entity type-block #f 3 2 #:color "light gray")
  (draw-entity type-block #f 2 1 #:color "light gray")
  (draw-entity type-block #f 2 3 #:color "light gray"))

(define (nearby draw-entity)
  (draw-entity type-bot #f 2 2)
  (draw-entity type-block #f 1 1 #:color "light gray")
  (draw-entity type-block #f 1 2 #:color "light gray")
  (draw-entity type-block #f 1 3 #:color "light gray")
  (draw-entity type-block #f 2 1 #:color "light gray")
  (draw-entity type-block #f 2 3 #:color "light gray")
  (draw-entity type-block #f 3 1 #:color "light gray")
  (draw-entity type-block #f 3 2 #:color "light gray")
  (draw-entity type-block #f 3 3 #:color "light gray"))

(define (bottom draw-entity)
  (draw-entity type-bot #f 0 49)
  (draw-entity type-bot #f 49 49))

;(viewer "test" nearby timer #:size 5 #:style '(no-caption))
(viewer "test" bottom timer)
