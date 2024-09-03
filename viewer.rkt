#lang racket

(require racket/gui/base)
(require "world.rkt" "location.rkt")

(let* ([world (make-world)]
       [bot1 (add-bot! world (location 1 5))]
       [bot2 (add-bot! world (location 1 6))]
       [bot3 (add-bot! world (location 2 5))]
       [frame (new frame% [label "robots"] [width 270] [height 250])]
       [font (make-font #:face "Courier")]
       [canvas
        (new canvas%
             [parent frame]
             [paint-callback
              (λ (canvas dc)
                (define row 0)
                (send dc set-font font )
                (for ([line (draw-world world 20)])
                  (send dc draw-text line 0 row)
                  (set! row (+ row 11))))])])
  (send frame show #t)
  (send canvas refresh-now #:flush? #t)
  
  (for ([i 10])
    (sleep .5)
    (move-bot! world (bot-id bot1) direction-east)
    (move-bot! world (bot-id bot2) direction-east)
    (move-bot! world (bot-id bot3) direction-east)
    (send canvas refresh-now #:flush? #t)))


