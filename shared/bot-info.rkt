#lang racket

(provide (struct-out bot-info) bot-info-bot-id)

(require "entity.rkt")

(struct bot-info (bot cargo neighbors) #:prefab)

(define (bot-info-bot-id info) (entity-id (bot-info-bot info)))
