#lang racket

(require "shared.rkt" "client/action.rkt" "local-connect.rkt"
         "server/setup.rkt" "client/viewer.rkt" "client/gathering.rkt")

(let* ([engine (setup-engine)]
       [draw-connection (connect-local engine)]
       [action-connection (connect-local engine)]
       [to-do (gathering-actions (action-connection request-hello))])
  (setup-server engine)
  (define (do-actions)
    (set! to-do (perform-actions action-connection to-do)))
  (viewer "robots - local" draw-connection do-actions))
