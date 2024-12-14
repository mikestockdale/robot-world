#lang racket/base

(provide players make-agent agent-score match-request add-to-score)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Agent}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/agent.rkt" "agent.rkt"]}
;An agent represents a body of code that interacts with the game.
;This may be a player client, or a game viewer client.

(struct agent ([type #:mutable] [score #:mutable]))
(define players '())
(define (make-agent) (agent 'unassigned 0))

;The first request sent to the agent @bold{set}s its @bold{type}.
;A draw request means the agent is a game viewer.
;A hello request means the agent is a game player.

(test-case:
 "set type"
 (define (check-type request)
   (let ([agent (make-agent)])
     (assign-agent-type! agent request)
     (agent-type agent)))
 (check-equal? (check-type request-draw) 'viewer)
 (check-equal? (check-type request-hello) 'player))

;If the agent type is unassigned, it means that the type hasn't been set yet.
;Once set, the type doesn't change.
 
(define (assign-agent-type! agent request)
  (when (equal? (agent-type agent) 'unassigned)
    (cond
      [(equal? request request-hello)
       (set-agent-type! agent 'player)
       (set! players (cons agent players))]
      [(equal? request request-draw) (set-agent-type! agent 'viewer)])))

;An agent checks if a @bold{request is valid}, based on the agent type.

(test-case:
 "valid request"
 (let ([agent (agent 'unassigned 0)])
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

;If the type hasn't been set yet, a draw or hello request is valid.
;For a game viewer, only draw requests are OK.
;For a game player, each request must be a list of commands.

(define (request-is-valid? agent request)
  (let ([type (agent-type agent)])
    (cond
      [(equal? type 'unassigned)
       (or (equal? request request-hello) (equal? request request-draw))]
      [(equal? type 'viewer) (equal? request request-draw)]
      [(equal? type 'player) (list? request)])))

;An agent @bold{match}es a @bold{request} by checking if the request is valid, and setting the agent type.

(test-case:
 "match request"
 (let ([agent (make-agent)])
   (check-false (match-request agent '(#f)))
   (check-equal? (agent-type agent) 'unassigned)
   (check-true (match-request agent request-draw))
   (check-equal? (agent-type agent) 'viewer)))

;If the request is not valid, the type is not changed.
;The result of the valid check is returned.

(define (match-request agent request)
  (let ([valid? (request-is-valid? agent request)])
    (when valid? (assign-agent-type! agent request))
    valid?))    

;When certain player events occur, an agent can bold{add to} its @bold{score}.

(test-case:
 "add to score"
 (let ([agent (agent 'player 0)])
   (add-to-score agent 3)
   (check-equal? (agent-score agent) 3)))

(define (add-to-score agent amount)
  (set-agent-score! agent (+ amount (agent-score agent))))
