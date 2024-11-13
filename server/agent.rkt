#lang racket

(provide make-agent delay!)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Agent}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/agent.rkt" "agent.rkt"]}
;An agent represents a body of code that interacts with the game.
;This may be a player client, a game viewer client, or a server module.
;
;The agent tracks the last time it was active.

(struct agent ([last-active-time #:mutable]))
(define (make-agent) (agent 0))

;An agent can be active only once every 100 milliseconds.
;It calculates the @bold{delay} required until the next active time.
;The system function @racket[current-inexact-milliseconds] is used as a timer.
;We can override this with a fake timer in our tests.

(define timer (make-parameter current-inexact-milliseconds))

(test-case:
 "delay is calculated"
 (define ((fake-timer value)) value)
 (let ([agent (make-agent)])
   (parameterize ([timer (fake-timer 1000)])
     (check-equal? (delay! agent) 0.0))
   (parameterize ([timer (fake-timer 1010)])
     (check-= (delay! agent) 0.09 .001))
   (parameterize ([timer (fake-timer 1105)])
     (check-= (delay! agent) 0.095 .001))))

;The delay is calculated as the difference from now to the last active time plus 100 milliseconds.
;The last active time is updated to now plus the delay.
;The return value is the delay in seconds.

(define (delay! agent)
  (let* ([now ((timer))] 
         [delay (max (+ (agent-last-active-time agent) 100.0 (- now)) 0.0)])
    (set-agent-last-active-time! agent (+ now delay))
    (/ delay 1000.0)))
