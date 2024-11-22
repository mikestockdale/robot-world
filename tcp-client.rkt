#lang racket

(require "shared.rkt" "client/action.rkt" "client/connection.rkt" "gathering.rkt")

(define (run)
  (let* ([connection (connect-remote "localhost" 8080)]
         [to-do (gathering-actions (connection request-hello))])
    (define (iterate)
      ;(sleep .1)
      (set! to-do (perform-actions connection to-do))
      (iterate))

    (iterate)))

(run)
