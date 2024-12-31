#lang racket

(provide background)

(require "board.rkt" "engine.rkt" "places.rkt" "shared.rkt")

;@title{Background}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/background.rkt" "background.rkt"]}

(define (blocks-in-bases places)
   (map-cargos
    places (λ (entity container-id)
           (and (equal? (occupant-type (occupant-by-id places container-id)) type-base)
                entity))))

(define (background engine)
  (sleep 10)
  (let* ([places (engine-places engine)]
         [blocks (blocks-in-bases places)])
    (unless (empty? blocks)
      (let ([random (random-location (engine-board engine))])
        (when (is-available? engine random)
          (place-entity places (first blocks) random)))))
  (background engine))
