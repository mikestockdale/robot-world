#lang racket/base

(provide viewer)
(require "shared.rkt")
(require racket/class racket/format racket/list racket/gui/base)
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

(define (viewer title connection action-procedure
                #:width [width 80] #:height [height 45] #:style [style '()] #:run [run #t])
  (define count 0)
  (define score "")
  (define colors (vector "Dark Blue" "Dark Red"))
  (set! run-actions run)
  (define (draw-procedure draw-entity)
    (let ([reply (connection request-draw)])
      (set! score (first reply))
      (for ([entity (rest reply)])
        (apply draw-entity entity))))
  (let* ([frame (new frame% [label title] [style style]
                     [width (* 10 width)] [height (+ (* 11 height) 22)])]
         [font (make-font #:face "DejaVu Sans Mono")]
         [canvas
          (new my-canvas%
               [parent frame]
               [paint-callback
                (λ (canvas dc)
                  (send dc set-font font)
                  (define (draw-entity entity-type team laden? x y #:color [color "black"])
                    (send dc set-text-foreground (if team (vector-ref colors team) color))
                    (send dc draw-text (string (entity-symbol entity-type laden?))
                          (* 10 x) (- (* 11 y) 6)))
                  (draw-procedure draw-entity))])])
    (define (iterate)
      (if run-actions
          (begin
            (action-procedure)
            (send frame set-status-text (~a count "   " score))
            (send canvas refresh-now #:flush? #t)
            (set! count (add1 count)))
          (sleep .001))
      (iterate))
    (send frame create-status-line)
    (send frame show #t)
    (thread iterate))) 
