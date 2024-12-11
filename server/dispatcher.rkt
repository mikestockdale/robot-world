#lang racket

(provide make-dispatcher dispatch-request)
(require "agent.rkt" "engine.rkt" "grid.rkt" "interval.rkt" "shared.rkt" "setup.rkt" "testing.rkt")
(module+ test (require rackunit))

;@title{Dispatcher}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/dispatcher.rkt" "dispatcher.rkt"]}
;The dispatcher takes requests from clients and executes the appropriate procedures on the server.
;It uses an agent to validate the requests, an engine to perform the requested actions and an interval to schedule their execution.

(struct dispatcher (engine agent interval))
(define (make-dispatcher engine) (dispatcher engine (make-agent) (make-interval)))

;A procedure is executed, based on the request

(define (execute-request engine request)
  (cond
    [(equal? request request-draw) (execute-draw engine)]
    [(equal? request request-hello) (execute-hello engine)]
    [else (dispatch-list engine request)]))

;A draw request returns a list of information for drawing entities.

(test-case:
 "entities are drawn"
 (test-engine
  ((size 3) (bot1 0 2) (block 2 1) (bot2 1 1))
  (check-equal? (execute-request engine request-draw)
                (list (list type-bot #f 0 2) (list type-block #f 2 1) (list type-bot #f 1 1)))))

;A draw reply is created from a list of all entities from the grid.

(define (execute-draw engine)
  (map-entities
   (engine-grid engine)
   (λ (occupant)
     (let* ([entity (occupant-entity occupant)]
            [cargo (entity-at (engine-grid engine) (entity-id entity))]
            [location (occupant-place occupant)])
       (list (entity-type entity)
             (if cargo #t #f)
             (location-x location)
             (location-y location))))))
   
;A hello request returns a list of new bots assigned to the client.

(test-case:
 "execute hello"
 (let ([reply (execute-request (make-engine 50) request-hello)])
   (check-true (andmap
                (λ (item) (equal? (entity-type (reply-entity item)) type-bot))
                reply)
               "returns new bots")))

;A hello request executes a procedure to set up bots.

(define (execute-hello engine)
  (map (λ (entity) ((make-reply #t (entity-id entity)) engine))
       (setup-bots engine)))

(define ((make-reply success? entity-id) engine)
  (let-values ([(occupant cargo neighbors) (entity-info engine entity-id)]) 
    (reply (if success? #t #f)
           (occupant-entity occupant)
           (occupant-place occupant)
           cargo neighbors)))
  
;A list of commands returns bot information for each command.

(test-case:
 "requests from player"
 (let* ([engine (make-engine 50)]
        [bot1 (add-entity engine type-bot (location 1 1))])
   (check-equal?
    (execute-request engine
                     (list (request request-move (entity-id bot1) direction-east)))
    (list (reply #t (entity 101 type-bot) (location 2 1) #f '())))))

;The engine procedure to be executed is accessed from a vector, based on the request type.

(define player-procedures (vector drop-entity move-entity take-entity transfer-entity))

(define (dispatch-list engine request-list)
  (define (execute request)
    (let* ([procedure (vector-ref player-procedures (request-type request))]
           [response (procedure engine
                                (request-id request)
                                (request-parameter request))])
      (make-reply response (request-id request))))
  (map (λ (make-reply) (make-reply engine)) (map execute request-list)))

;When the dispatcher @bold{dispatch}es a @bold{request}, an invalid request returns an error message.

(test-case:
 "invalid request"
 (check-equal? (dispatch-request (make-dispatcher (make-engine 5)) '(#f))
               "invalid request"))

;The dispatcher delays execution to limit the rate of execution for each client.
;If the agent matches the request, it is executed.
;Otherwise, a message is returned.

(define (dispatch-request dispatcher request)
  ((dispatcher-interval dispatcher))
  (if (match-request (dispatcher-agent dispatcher) request)
      (execute-request (dispatcher-engine dispatcher) request)
      "invalid request"))
