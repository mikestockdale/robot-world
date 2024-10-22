#lang racket

(require "action.rkt" "connection.rkt" "wandering.rkt")

(define (run)
  (let* ([connection (connect-remote "localhost" 8080)]
         [to-do (wandering-actions (send-hello connection))])
    (define (iterate)
      (sleep .1)
      (set! to-do (perform-actions connection to-do))
      (iterate))

    (iterate)))

(run)
