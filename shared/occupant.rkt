#lang racket

(provide (struct-out occupant) is-cargo? at-location?)

(struct occupant (entity place) #:prefab)

(define (is-cargo? place) (number? place))
(define (at-location? place) (not (is-cargo? place)))
