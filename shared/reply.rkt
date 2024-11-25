#lang racket

(provide (struct-out reply))

;@title{Reply}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/reply.rkt" "reply.rkt"]}
;A reply from the server to a player client contains a success indicator and bot information.

(struct reply (success? entity cargo neighbors) #:prefab)
