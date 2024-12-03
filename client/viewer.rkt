#lang racket

(provide viewer)
(require "shared.rkt")
(require racket/gui/base)
(module+ test (require rackunit))

;@title{Viewer}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/viewer.rkt" "viewer.rkt"]}
;The viewer displays an animation of the game actions.

;To display an entity, we assign an @bold{entity symbol}.
;The bot symbol is different if it is laden with a block.

(test-case:
 "entity symbols"
 (check-equal? (entity-symbol type-bot #f) #\u25A1)
 (check-equal? (entity-symbol type-bot 'laden) #\u25A3)
 (check-equal? (entity-symbol type-block #f) #\u25A0)
 (check-equal? (entity-symbol type-base #f) #\u25A4)
 (check-equal? (entity-symbol type-edge #f) #\?))

;Only bases, bots,and blocks should appear. Anything else is a mistake!

(define (entity-symbol entity-type laden?)
  (cond
    [(= entity-type type-bot) (if laden? #\u25A3  #\u25A1)]
    [(= entity-type type-block) #\u25A0]
    [(= entity-type type-base) #\u25A4]
    [else #\?]))

;The animation can be paused and resumed with a mouse click.

(define run-actions #t)

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (when (send event button-down?)
        (set! run-actions (not run-actions)))) 
    (super-new)))

;The animation is displayed.

(define (viewer title draw-procedure action-procedure
                #:size [size 50] #:style [style '()] #:run [run #t])
  (define count 0)
  (set! run-actions run)
  (let* ([frame (new frame% [label title] [style style]
                     [width (* 10 size)] [height (+ (* 11 size) 22)])]
         [font (make-font #:face "DejaVu Sans Mono")]
         [canvas
          (new my-canvas%
               [parent frame]
               [paint-callback
                (λ (canvas dc)
                  (send dc set-font font )
                  (define (draw-entity entity-type laden? x y #:color [color "black"])
                    (send dc set-text-foreground color)
                    (send dc draw-text (string (entity-symbol entity-type laden?))
                          (* 10 x) (- (* 11 y) 6)))
                  (draw-procedure draw-entity))])])
    (define (iterate)
      (if run-actions
          (begin
            (action-procedure)
            (send frame set-status-text (number->string count))
            (send canvas refresh-now #:flush? #t)
            (set! count (add1 count)))
          (sleep .001))
      (iterate))
    (send frame create-status-line)
    (send frame show #t)
    (thread iterate))) 
