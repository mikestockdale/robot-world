#lang racket

(provide agent delay!)

(struct agent ([last-active-time #:mutable]))

(define timer (make-parameter current-inexact-milliseconds))

(define (delay! agent)
  (let* ([now ((timer))] 
         [delay (max (+ (agent-last-active-time agent) 100.0 (- now)) 0.0)])
    (set-agent-last-active-time! agent (+ now delay))
    (/ delay 1000.0))) 

(module+ test
  (require rackunit)

  (test-case
   "delay is calculated"
   (define ((fake-timer value)) value)
   (let ([agent (agent 0)])
     (parameterize ([timer (fake-timer 1000)])
       (check-equal? (delay! agent) 0.0))
     (parameterize ([timer (fake-timer 1010)])
       (check-= (delay! agent) 0.09 .001))
     (parameterize ([timer (fake-timer 1105)])
       (check-= (delay! agent) 0.095 .001))))

  )