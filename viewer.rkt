#lang racket

(require racket/gui/base)
(require "actions.rkt" "entity.rkt" "server.rkt" "world.rkt" "location.rkt" "wandering.rkt")

(define (run-viewer world actions)
  (define run-actions #t)
  (define to-do actions)
  (let* ([frame (new frame% [label "robots"] [width 500] [height 555])]
         [font (make-font #:face "Courier")]
         [my-canvas%
          (class canvas%
            (define/override (on-event event)
              (when (send event button-down?)
                (set! run-actions (not run-actions)))) 
            (super-new))]
         [canvas
          (new my-canvas%
               [parent frame]
               [paint-callback
                (λ (canvas dc)
                  (define row 0)
                  (send dc set-font font )
                  (for ([line (draw-world world)])
                    (define col 0)
                    (for ([char (string->list line)])
                      (send dc draw-text (string char) col row)
                      (set! col (+ col 10)))
                    (set! row (+ row 11))))])])
    (send frame show #t)
    (send canvas refresh-now #:flush? #t)

    (define timer
      (new timer%
           [interval 100]
           [notify-callback
            (λ () 
              (when run-actions
                (set! to-do (perform-actions to-do))
                (send canvas refresh-now #:flush? #t)))]))
    #t))

(let* ([world (make-world 50)]
       [server (connect-server world)]
       [actions
        (actions server
                 (list
                  (make-wandering server (location 20 20) direction-east)
                  (make-wandering server (location 30 30) direction-west)))])
  (add-entity! world type-block (location 25 25))
  (add-entity! world type-block (location 35 25))
  (add-entity! world type-block (location 25 35))
  (add-entity! world type-block (location 45 25))
  (add-entity! world type-block (location 25 45))
  (add-entity! world type-block (location 15 25))
  (add-entity! world type-block (location 25 15))
  (add-entity! world type-block (location 5 25))
  (add-entity! world type-block (location 25 5))
  (run-viewer world actions))
