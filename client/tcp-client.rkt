#lang racket

(require "shared.rkt" "step.rkt" "connection.rkt" "wandering.rkt")

;@title{TCP Client}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/tcp-client.rkt" "tcp-client.rkt"]}
;The TCP client connects to a TCP server and sets up a list of actions.
;It continuously performs the actions, which generates a new set of actions.

(define (run)
  (let* ([connection (connect-remote "localhost" 8080)])
    (define (iterate actions)
      (iterate (perform-steps connection actions)))
    (iterate (wandering-steps (connection request-hello)))))

(run)
