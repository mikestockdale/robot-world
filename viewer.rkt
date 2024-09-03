#lang racket

(require racket/gui/base)
(require "world.rkt" "location.rkt")

(define (go-east! world bot-id) (move-bot! world bot-id direction-east))

(struct actions (world list))
(struct action (bot-id procedure))

(define (make-actions world . setups)
  (define (make-action setup)
    (action (bot-id (add-bot! world (first setup))) (second setup)))
  (actions world (map make-action setups)))

(define (perform-actions actions)
  (for-each
   (λ (action)
     ((action-procedure action) (actions-world actions) (action-bot-id action)))
   (actions-list actions)))

(define (run-viewer actions)
  (define run-actions #t)
  (let* ([frame (new frame% [label "robots"] [width 500] [height 550])]
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
                  (for ([line (draw-world (actions-world actions))])
                    (send dc draw-text line 0 row)
                    (set! row (+ row 11))))])])
    (send frame show #t)
    (send canvas refresh-now #:flush? #t)

    (define timer
      (new timer%
           [interval 500]
           [notify-callback
            (λ () 
              (when run-actions
                (perform-actions actions)
                (send canvas refresh-now #:flush? #t)))]))
    #t))

(let* ([world (make-world 50)]
       [actions
        (make-actions world
                      (list (location 0 0) go-east!)
                      (list (location 0 5) go-east!)
                      (list (location 1 4) go-east!))])
  (run-viewer actions))
