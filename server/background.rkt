#lang racket

(provide background)

(require "board.rkt" "engine.rkt" "grid.rkt" "shared.rkt")

;@title{Background}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/background.rkt" "background.rkt"]}

(define (blocks-in-bases grid)
  (flatten
   (map-entities
    grid (λ (occupant)
           (and (equal? (entity-type (occupant-entity occupant)) type-base)
                (entity-at grid (entity-id (occupant-entity occupant))))))))

(define (background engine)
  (sleep 10)
  (let* ([grid (engine-grid engine)]
         [blocks (blocks-in-bases grid)])
    (unless (empty? blocks)
      (let ([random (random-location (engine-board engine))])
        (when (is-available? engine random)
          (place-entity grid (first blocks) random)))))
  (background engine))
