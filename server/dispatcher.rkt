#lang racket

(provide make-dispatcher dispatch-request)
(require "engine.rkt" "shared.rkt" "../setup.rkt")

(struct dispatcher (engine [procedure #:mutable]))
(define (make-dispatcher engine) (dispatcher engine dispatch-first-request))

(define (dispatch-request dispatcher request)
  ((dispatcher-procedure dispatcher) dispatcher request))
  
(define (dispatch-first-request dispatcher request)
  (cond
    [(eq? request 'draw)
     (set-dispatcher-procedure! dispatcher dispatch-draw)
     (draw-entities (dispatcher-engine dispatcher))]
    [(eq? request 'hello)
     (set-dispatcher-procedure! dispatcher dispatch-player)
     (execute-hello (dispatcher-engine dispatcher))]
    [else "draw or hello"]))

(define (dispatch-draw dispatcher request)
  (if (eq? request 'draw)
      (draw-entities (dispatcher-engine dispatcher))
      "draw only"))

(define player-procedures (vector drop-entity move-entity take-entity))

(define (dispatch-player dispatcher request-list)
  (define (execute request)
    (let ([procedure (vector-ref player-procedures (request-type request))])
      (if procedure
          (make-response-list
           (procedure (dispatcher-engine dispatcher) (request-id request) (request-parameter request))
           (request-id request)
           (dispatcher-engine dispatcher))
          "player only")))
  (map execute request-list))

(define (execute-hello engine)
  (map (λ (bot) (make-response-list #t (entity-id bot) engine))
       (setup-bots engine)))

(define (make-response-list success? entity-id engine)
  (list (if success? #t #f) (make-bot engine entity-id)))

