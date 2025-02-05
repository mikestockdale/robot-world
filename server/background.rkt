#lang racket

(provide background)

(require "board.rkt" "engine.rkt" "places.rkt" "shared.rkt")

;@title{Background}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/background.rkt" "background.rkt"]}
;A @bold{background} task is running to randomly place blocks on the board.
;The blocks are randomly chosen from ones that have been transferred to bases.

(define (blocks-in-bases places)
   (filter-map-cargos
    places (λ (entity container-id)
           (and (equal? (occupant-type (occupant-by-id places container-id)) type-base)
                entity))))

(define (background engine)
  (sleep 10)
  (let* ([places (engine-places engine)]
         [blocks (blocks-in-bases places)])
    (unless (empty? blocks)
      (let ([random (random-location (engine-board engine))])
        (when (available? engine random)
          (place-entity places (first blocks) random)))))
  (background engine))
