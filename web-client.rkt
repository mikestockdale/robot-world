#lang racket

(require "actions.rkt" "direction.rkt" "server.rkt" "location.rkt" "wandering.rkt")

(define (run)
  (let* ([server (connect-remote "localhost" 8080)]
         [to-do
          (actions server
                   (list
                    (make-wandering server (location 20 20) direction-east)
                    (make-wandering server (location 30 30) direction-west)))])
    (define (iterate)
      (sleep .1)
      (set! to-do (perform-actions to-do))
      (iterate))

    (iterate)))

(run)
