#lang racket

(require "shared.rkt" "client/action.rkt" "local-connect.rkt"
         "server/setup.rkt" "client/viewer.rkt" "client/wandering.rkt")

(let* ([engine (setup-engine)]
       [draw-connection (connect-local engine)]
       [action1-connection (connect-local engine)]
       [to-do1 (wandering-actions (action1-connection request-hello))]
       [action2-connection (connect-local engine)]
       [to-do2 (wandering-actions (action2-connection request-hello))])
  (setup-server engine)
  (define (do-actions)
    (set! to-do1 (perform-actions action1-connection to-do1))
    (set! to-do2 (perform-actions action2-connection to-do2)))
  (viewer "robots - local" draw-connection do-actions))
