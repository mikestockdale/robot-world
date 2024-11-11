#lang racket

(provide make-agent process-request)
(require "dispatch.rkt" "shared.rkt")
(module+ test (require rackunit))

(struct agent (dispatcher [last-active-time #:mutable]))
(define (make-agent engine) (agent (make-dispatch engine) 0))

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

(define (process-request agent request)
  (sleep (delay! agent))
  (dispatch-request (agent-dispatcher agent) request))
