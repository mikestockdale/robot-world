#lang racket

(provide setup-server setup-bots setup-engine)
(require "background.rkt" "board.rkt" "shared.rkt" "engine.rkt")

;@title{Setup}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/setup.rkt" "setup.rkt"]}
;Setup creates an engine and populates it with blocks, and creates bots for a player.

(define (setup-engine)
  (make-engine 80 45))

(define (add-random-location engine entity-type)
  (let ([new-entity (add-entity engine entity-type (random-location (engine-board engine)))])
    (if new-entity
        new-entity
        (add-random-location engine entity-type))))

(define (setup-server engine)
  (for ([i 25])
    (add-random-location engine type-block))
  (thread (λ () (background engine)))
  #t)

;To set up bots, a base is created in a random location.
;The bots are added at locations adjacent to the base.

(define (setup-bots engine)
  (let ([base-location (random-base (engine-board engine)
                               (λ (x) (available? engine x)))])
    (add-entity engine type-base base-location)
    (for/list ([location (all-directions base-location)])
      (add-entity engine type-bot location))))
