#lang racket

(provide make-dispatcher dispatch-request)
(require "agent.rkt" "engine.rkt" "shared.rkt" "../setup.rkt")
(module+ test (require rackunit))

;@title{Dispatcher}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/dispatcher.rkt" "dispatcher.rkt"]}

(struct dispatcher (engine agent))
(define (make-dispatcher engine) (dispatcher engine (make-agent)))

(test-case:
 "dispatch first request"
 (define (first-request request)
   (dispatch-request (make-dispatcher (make-engine 50)) request))
 (check-equal? (first-request request-hello) (execute-hello (make-engine 50))) 
 (check-equal? (first-request request-draw) '() "nothing to draw")
 (check-equal? (first-request '(#f)) "invalid request"))

(define (dispatch-request dispatcher request)
  (let ([delay (delay! (dispatcher-agent dispatcher))])
    (sleep delay))
  (if (request-is-valid? (dispatcher-agent dispatcher) request)
      (begin
        (set-type! (dispatcher-agent dispatcher) request)
        (cond
          [(equal? request request-draw) (draw-entities (dispatcher-engine dispatcher))]
          [(equal? request request-hello) (execute-hello (dispatcher-engine dispatcher))]
          [else (dispatch-list (dispatcher-engine dispatcher) request)]))
      "invalid request"))
  
(define (execute-hello engine)
  (map (λ (bot) (make-response-list #t (entity-id bot) engine))
       (setup-bots engine)))

(define (make-response-list success? entity-id engine)
  (list (if success? #t #f) (make-bot engine entity-id)))

(test-case:
 "requests from player"
 (parameterize ([agent-interval 0.0])
   (let* ([engine (make-engine 50)]
          [bot1 (add-entity engine type-bot (location 1 1))]
          [dispatcher (make-dispatcher engine)])
     (dispatch-request dispatcher request-hello)
     (check-equal?
      (dispatch-request dispatcher
                        (list (request request-move (entity-id bot1) direction-east)))
      (list (list #t (bot (entity 101 type-bot (location 2 1)) #f '())))))))

(define player-procedures (vector drop-entity move-entity take-entity))

(define (dispatch-list engine request-list)
  (define (execute request)
    (let* ([procedure (vector-ref player-procedures (request-type request))]
           [response (procedure engine
                                (request-id request)
                                (request-parameter request))])
      (make-response-list response
                          (request-id request)
                          engine)))
  (map execute request-list))
