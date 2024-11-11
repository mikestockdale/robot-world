#lang racket

(provide make-agent dispatch-request)
(require "engine.rkt" "shared.rkt" "../setup.rkt")
(module+ test (require rackunit))

(struct agent (engine [dispatcher #:mutable] [last-active-time #:mutable]))
(define (make-agent engine) (agent engine dispatch-first-request 0))

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

(define timer (make-parameter current-inexact-milliseconds))

(define (delay! agent)
  (let* ([now ((timer))] 
         [delay (max (+ (agent-last-active-time agent) 100.0 (- now)) 0.0)])
    (set-agent-last-active-time! agent (+ now delay))
    (/ delay 1000.0)))

(define (dispatch-request agent request)
  (sleep (delay! agent))
  ((agent-dispatcher agent) agent request))

(define (dispatch-first-request agent request)
  (cond
    [(eq? request 'draw)
     (set-agent-dispatcher! agent dispatch-draw)
     (draw-entities (agent-engine agent))]
    [(eq? request 'hello)
     (set-agent-dispatcher! agent dispatch-player)
     (execute-hello (agent-engine agent))]
    [else "draw or hello"]))

(define (dispatch-draw agent request)
  (if (eq? request 'draw)
      (draw-entities (agent-engine agent))
      "draw only"))

(define player-procedures (vector drop-entity move-entity take-entity))

(define (dispatch-player agent request-list)
  (define (execute request)
    (let ([procedure (vector-ref player-procedures (request-type request))])
      (if procedure
          (make-response-list
           (procedure (agent-engine agent) (request-id request) (request-parameter request))
           (request-id request)
           (agent-engine agent))
          "player only")))
  (map execute request-list))

(define (execute-hello engine)
  (map (λ (bot) (make-response-list #t (entity-id bot) engine))
       (setup-bots engine)))

(define (make-response-list success? entity-id engine)
  (list (if success? #t #f) (make-bot engine entity-id)))


