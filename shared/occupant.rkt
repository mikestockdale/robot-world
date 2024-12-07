#lang racket

(provide (struct-out occupant))

(struct occupant (entity place) #:prefab)
