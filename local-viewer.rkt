#lang racket

(require "shared.rkt" "client/action.rkt" "local-connect.rkt"
         "server/setup.rkt" "client/viewer.rkt" "client/gathering.rkt")

(let* ([engine (setup-engine)]
       [draw-connection (connect-local engine)]
       [action-connection (connect-local engine)]
       [to-do (gathering-actions (action-connection request-hello))])
  (setup-blocks engine)

  (define (draw-procedure draw-entity)
    (for ([entity (draw-connection request-draw)])
      (apply draw-entity entity)))

  (define (do-actions)
    (set! to-do (perform-actions action-connection to-do)))
  
  (viewer "robots - local" draw-procedure do-actions))
