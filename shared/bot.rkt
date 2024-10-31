#lang racket

(provide (struct-out bot) bot-id bot-location)

(require "entity.rkt")

(struct bot (entity cargo neighbors) #:prefab)

(define (bot-id bot) (entity-id (bot-entity bot)))
(define (bot-location bot) (entity-location (bot-entity bot)))
