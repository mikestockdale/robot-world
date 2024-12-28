#lang racket/base

(provide (struct-out neighbor))

(struct neighbor (entity location) #:prefab)
