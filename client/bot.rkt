#lang racket

(provide (struct-out bot) make-bot bot-id bot-location)

(require "shared.rkt")

;@title{Bot}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/bot.rkt" "bot.rkt"]}
;A bot is an entity that is controlled by client code.
;It can pick up a block entity.
;This is stored in the @racket[cargo] field.
;It also has a list of @elemref["nearby"]{nearby} entities that it can see, in the @racket[neighbors] field.

(struct bot (entity cargo neighbors))
(define (make-bot reply) (bot (reply-entity reply) (reply-cargo reply) (reply-neighbors reply)))

;We include a couple of helper functions, to access the entity id and location data.

(define (bot-id bot) (entity-id (bot-entity bot)))
(define (bot-location bot) (entity-location (bot-entity bot)))
