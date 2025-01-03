#lang racket/base

(provide (struct-out reply))

;@title{Reply}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/reply.rkt" "reply.rkt"]}
;A reply from the server to a player client contains a success indicator, the bot id from the request, its location, its cargo, if any, and a list of @elemref["nearby"]{nearby} entities.

(struct reply (success? id location cargo neighbors) #:prefab)
