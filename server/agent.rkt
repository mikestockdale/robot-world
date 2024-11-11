#lang racket

(provide make-agent process-request)
(require "dispatcher.rkt" "shared.rkt")
(module+ test (require rackunit))

;@title{Agent}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/agent.rkt" "agent.rkt"]}
;An agent represents a body of code that interacts with the game.
;This may be a player client, a game viewer client, or a server module.
;
;The agent manages incoming requests. It uses a dispatcher to process them, and it tracks the last time it was active.

(struct agent (dispatcher [last-active-time #:mutable]))
(define (make-agent engine) (agent (make-dispatcher engine) 0))

;An agent can be active only once every 100 milliseconds.
;It calculates the @bold{delay} required until the next active time.
;The system function @racket[current-inexact-milliseconds] is used as a timer.
;We can override this with a fake timer in our tests.

(define timer (make-parameter current-inexact-milliseconds))

(test-case:
 "delay is calculated"
 (define ((fake-timer value)) value)
 (let ([agent (make-agent #f)])
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

;When the agent @bold{process}es a @bold{request}, it sleeps for the delay time and then dispatches the request.

(define (process-request agent request)
  (sleep (delay! agent))
  (dispatch-request (agent-dispatcher agent) request))
