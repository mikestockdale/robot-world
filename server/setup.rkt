#lang racket

(provide setup-blocks setup-bots setup-engine)
(require "shared.rkt" "engine.rkt")

;@title{Setup}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/setup.rkt" "setup.rkt"]}
;Setup creates an engine, populates it with blocks, and creates bots for a player.

(define (setup-engine)
  (make-engine 50))

(define (add-random-location engine entity-type)
  (let ([new-entity (add-entity engine entity-type (location (random 50) (random 50)))])
    (if new-entity
        new-entity
        (add-random-location engine entity-type))))

(define (setup-blocks engine)
  (for ([i 25])
    (add-random-location engine type-block))
  #t)

(define (setup-bots engine)
  (let ([base (add-random-base engine)])
    (for/list ([location (all-directions (entity-location base))])
      (add-entity engine type-bot location))))
