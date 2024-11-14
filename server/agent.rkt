#lang racket

(provide make-agent agent-interval delay! request-is-valid? set-type!)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Agent}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/agent.rkt" "agent.rkt"]}
;An agent represents a body of code that interacts with the game.
;This may be a player client, or a game viewer client.
;The agent tracks the last time it was active.

(struct agent ([type #:mutable] [last-active-time #:mutable]))
(define (make-agent) (agent 'none 0))

;The first request sent to the agent @bold{set}s its @bold{type}.
;A draw request means the agent is a game viewer.
;A hello request means the agent is a game player.

(test-case:
 "set type"
 (define (check-type request)
   (let ([agent (make-agent)])
     (set-type! agent request)
     (agent-type agent)))
 (check-equal? (check-type request-draw) 'viewer)
 (check-equal? (check-type request-hello) 'player))

;If the agent type is none, it means that the type hasn't been set yet.
;Once set, the type doesn't change.
 
(define (set-type! agent request)
  (when (equal? (agent-type agent) 'none)
    (cond
      [(equal? request request-hello) (set-agent-type! agent 'player)]
      [(equal? request request-draw) (set-agent-type! agent 'viewer)])))

;An agent checks if a @bold{request is valid}, based on the agent type.
;If the type hasn't been set yet, a draw or hello request is valid.
;For a game viewer, only draw requests are OK.
;For a game player, each request must be a list of commands.

(test-case:
 "valid request"
 (let ([agent (agent 'none 0)])
   (check-true (request-is-valid? agent request-draw))
   (check-true (request-is-valid? agent request-hello))
   (check-false (request-is-valid? agent '(#f))))
 (let ([agent (agent 'viewer 0)])
   (check-true (request-is-valid? agent request-draw))
   (check-false (request-is-valid? agent request-hello))
   (check-false (request-is-valid? agent '(#f))))
 (let ([agent (agent 'player 0)])
   (check-false (request-is-valid? agent request-draw))
   (check-false (request-is-valid? agent request-hello))
   (check-true (request-is-valid? agent '(#f)))))

(define (request-is-valid? agent request)
  (let ([type (agent-type agent)])
    (cond
      [(equal? type 'none)
       (or (equal? request request-hello) (equal? request request-draw))]
      [(equal? type 'viewer) (equal? request request-draw)]
      [(equal? type 'player) (list? request)])))

;An agent can be active only once every 100 milliseconds.
;It calculates the @bold{delay} required until the next active time.
;The system function @racket[current-inexact-milliseconds] is used as a timer.
;We can override this with a fake timer in our tests.

(define timer (make-parameter current-inexact-milliseconds))
(define agent-interval (make-parameter 100.0))

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
         [delay
           (max (+ (agent-last-active-time agent) (agent-interval) (- now)) 0.0)])
    (set-agent-last-active-time! agent (+ now delay))
    (/ delay 1000.0)))
