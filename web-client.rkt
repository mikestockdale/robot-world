#lang racket

(require "action.rkt" "direction.rkt" "server.rkt" "location.rkt" "wandering.rkt")

(define (run)
  (let* ([server (connect-remote "localhost" 8080)]
         [to-do (map wandering-action (hello server))])
    (define (iterate)
      (sleep .1)
      (set! to-do (perform-actions server to-do))
      (iterate))

    (iterate)))

(run)
