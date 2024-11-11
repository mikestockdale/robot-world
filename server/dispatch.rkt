#lang racket

(provide make-dispatch dispatch-request)
(require "engine.rkt" "shared.rkt" "../setup.rkt")

(struct dispatch (engine [procedure #:mutable]))
(define (make-dispatch engine) (dispatch engine dispatch-first-request))

(define (dispatch-request dispatch request)
  ((dispatch-procedure dispatch) dispatch request))
  
(define (dispatch-first-request dispatch request)
  (cond
    [(eq? request 'draw)
     (set-dispatch-procedure! dispatch dispatch-draw)
     (draw-entities (dispatch-engine dispatch))]
    [(eq? request 'hello)
     (set-dispatch-procedure! dispatch dispatch-player)
     (execute-hello (dispatch-engine dispatch))]
    [else "draw or hello"]))

(define (dispatch-draw dispatch request)
  (if (eq? request 'draw)
      (draw-entities (dispatch-engine dispatch))
      "draw only"))

(define player-procedures (vector drop-entity move-entity take-entity))

(define (dispatch-player dispatch request-list)
  (define (execute request)
    (let ([procedure (vector-ref player-procedures (request-type request))])
      (if procedure
          (make-response-list
           (procedure (dispatch-engine dispatch) (request-id request) (request-parameter request))
           (request-id request)
           (dispatch-engine dispatch))
          "player only")))
  (map execute request-list))

(define (execute-hello engine)
  (map (λ (bot) (make-response-list #t (entity-id bot) engine))
       (setup-bots engine)))

(define (make-response-list success? entity-id engine)
  (list (if success? #t #f) (make-bot engine entity-id)))

