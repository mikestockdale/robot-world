#lang racket

(require "action.rkt" "connection.rkt" "gathering.rkt")

(define (run)
  (let* ([connection (connect-remote "127.0.0.1" 8080)]
         [to-do (gathering-actions (send-hello connection))])
    (define (iterate)
      (sleep .1)
      (set! to-do (perform-actions connection to-do))
      (iterate))

    (iterate)))

(run)
