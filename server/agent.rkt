#lang racket

(provide make-agent validate-request)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Agent}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/agent.rkt" "agent.rkt"]}
;An agent represents a body of code that interacts with the game.
;This may be a player client, or a game viewer client.

(struct agent ([type #:mutable]))
(define (make-agent) (agent 'none))

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

;An agent checks if a @bold{request matches} the agent type.

(test-case:
 "valid request"
 (let ([agent (agent 'none)])
   (check-true (request-matches? agent request-draw))
   (check-true (request-matches? agent request-hello))
   (check-false (request-matches? agent '(#f))))
 (let ([agent (agent 'viewer)])
   (check-true (request-matches? agent request-draw))
   (check-false (request-matches? agent request-hello))
   (check-false (request-matches? agent '(#f))))
 (let ([agent (agent 'player)])
   (check-false (request-matches? agent request-draw))
   (check-false (request-matches? agent request-hello))
   (check-true (request-matches? agent '(#f)))))

;If the type hasn't been set yet, a draw or hello request is valid.
;For a game viewer, only draw requests are OK.
;For a game player, each request must be a list of commands.

(define (request-matches? agent request)
  (let ([type (agent-type agent)])
    (cond
      [(equal? type 'none)
       (or (equal? request request-hello) (equal? request request-draw))]
      [(equal? type 'viewer) (equal? request request-draw)]
      [(equal? type 'player) (list? request)])))

(test-case:
 "something"
 (check-true #t))

(define (validate-request agent request)
  (let ([matches? (request-matches? agent request)])
    (when matches? (set-type! agent request))
    matches?))    
