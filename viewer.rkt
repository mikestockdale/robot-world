#lang racket

(provide run-viewer)
(require racket/gui/base)

(define (run-viewer draw-procedure timer-procedure)
  (define run-actions #t)
  (let* ([frame (new frame% [label "robots"] [width 500] [height 549])]
         [font (make-font #:face "DejaVu Sans Mono")]
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
                  (send dc set-font font )
                  (define (draw-entity symbol x y)
                    (send dc draw-text (string symbol) (* 10 x) (- (* 11 y) 6)))
                  (draw-procedure draw-entity))])])
    (send frame show #t)
    (send canvas refresh-now #:flush? #t)

    (define timer
      (new timer%
           [interval 100]
           [notify-callback
            (λ () 
              (when run-actions
                (timer-procedure)
                (send canvas refresh-now #:flush? #t)))]))
    #t))
