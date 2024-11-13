#lang racket

(provide make-dispatcher dispatch-request)
(require "agent.rkt" "engine.rkt" "shared.rkt" "../setup.rkt")
(module+ test (require rackunit))

;@title{Dispatcher}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/dispatcher.rkt" "dispatcher.rkt"]}

(struct dispatcher (engine agent [last-request #:mutable]))
(define (make-dispatcher engine) (dispatcher engine (make-agent) 'none))

(test-case:
 "valid and invalid request order"
 (check-true (is-valid? request-hello 'none))
 (check-false (is-valid? request-hello request-draw))
 (check-false (is-valid? request-hello request-hello))
 (check-false (is-valid? request-hello '(#f)))
 (check-true (is-valid? request-draw 'none))
 (check-true (is-valid? request-draw request-draw))
 (check-false (is-valid? request-draw request-hello))
 (check-false (is-valid? request-draw (list #f)))
 (check-true (is-valid? '(#f) request-hello))
 (check-true (is-valid? '(#f) '(#f)))
 (check-false (is-valid? '(#f) 'none))
 (check-false (is-valid? '(#f) request-draw)))

(define (is-valid? request last-request)
  (cond
    [(equal? request request-hello) (equal? last-request 'none)]
    [(equal? request request-draw)
     (or (equal? last-request 'none) (equal? last-request request-draw))]
    [else (or (equal? last-request request-hello) (list? last-request))]))

(test-case:
 "dispatch first request"
 (define (test-request request)
   (dispatch-request (make-dispatcher (make-engine 50)) request))
 (check-equal? (length (test-request request-hello)) 4 "bots created") 
 (check-equal? (test-request request-draw) '() "nothing to draw")
 (check-equal? (test-request '(#f)) "invalid request" "invalid"))

(define (dispatch-request dispatcher request)
  (let ([delay (delay! (dispatcher-agent dispatcher))])
    (sleep delay))
  (if (is-valid? request (dispatcher-last-request dispatcher))
      (begin
        (set-dispatcher-last-request! dispatcher request)
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
 (define (next-request the-dispatcher request)
   (dispatch-request
    (struct-copy dispatcher the-dispatcher [agent (make-agent)]) request)) 
 (let* ([engine (make-engine 50)]
        [bot (add-entity engine type-bot (location 1 1))]
        [dispatcher (make-dispatcher engine)])
   (dispatch-request dispatcher request-hello)
   (check-equal?
    (length
     (next-request dispatcher
                   (list (request request-move (entity-id bot) direction-east))))
    1 "response")))

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
