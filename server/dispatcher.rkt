#lang racket/base

(provide make-dispatcher)
(require "agent.rkt" "draw.rkt" "engine.rkt" "interval.rkt" "shared.rkt" "setup.rkt")
(module+ test (require rackunit))

;@title{Dispatcher}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/dispatcher.rkt" "dispatcher.rkt"]}
;The dispatcher takes requests from clients and executes the appropriate procedures on the server.
;It uses an agent to validate the requests, and an engine to perform the requested actions.

;The dispatcher uses an interval delays execution to limit the rate of execution for each client.

(define (make-dispatcher)
  (let ([agent (make-agent)]
        [interval (make-interval)])
    (λ (engine request)
      (interval)
      (dispatch-request engine agent request))))

;An invalid request returns an error message.

(test-case:
 "invalid request"
 (check-equal? (dispatch-request (make-engine 4 5) (make-agent) '(#f))
               "invalid request"))

;If the agent matches the request, a procedure is executed, based on the request
;Otherwise, a message is returned.

(define (dispatch-request engine agent request)
  (if (match-request agent request)
      (cond
        [(equal? request request-draw) (draw-entities players (engine-grid engine))]
        [(equal? request request-hello) (execute-hello engine agent)]
        [else (dispatch-list engine agent request)])
      "invalid request"))

;A hello request returns a list of new bots assigned to the client.

(test-case:
 "execute hello"
 (let* ([agent (make-agent)]
        [reply (dispatch-request (make-engine 40 50) agent request-hello)])
   (check-true (andmap
                (λ (item)
                  (let ([bot (reply-entity item)])
                    (and
                     (equal? (entity-type bot) type-bot)
                     (equal? (find-agent (list agent) (entity-id bot)) 0))))
                reply)
               "returns new bots")))

;A hello request executes a procedure to set up bots.

(define (execute-hello engine agent)
  (let ([bots (setup-bots engine)])
    (assign-bots! agent (map entity-id bots))
    (map (λ (entity) ((make-reply #t (entity-id entity)) engine))
         bots)))

(define ((make-reply success? entity-id) engine)
  (let-values ([(occupant cargo neighbors) (entity-info engine entity-id)]) 
    (reply (if success? #t #f)
           (occupant-entity occupant)
           (occupant-location occupant)
           cargo neighbors)))
  
;A list of commands returns bot information for each command.

(test-case:
 "requests from player"
 (let* ([engine (make-engine 4 5)]
        [bot1 (add-entity engine type-bot (location 1 1))]
        [agent (make-agent)])
   (match-request agent request-hello)
   (check-equal?
    (dispatch-request engine agent
                      (list (request request-move (entity-id bot1) (location 2 1))))
    (list (reply #t (entity 101 type-bot) (location 2 1) #f '())))))

;The engine procedure to be executed is accessed from a vector, based on the request type.

(define player-procedures (vector drop-entity move-entity take-entity transfer-entity))

(define (dispatch-list engine agent request-list)
  (define (execute request)
    (let* ([procedure (vector-ref player-procedures (request-type request))]
           [success? (procedure engine
                                (request-id request)
                                (request-parameter request))])
      (when (and success? (= (request-type request) request-transfer))
        (add-to-score agent 1))
      (make-reply success? (request-id request))))
  (map (λ (make-reply) (make-reply engine)) (map execute request-list)))
