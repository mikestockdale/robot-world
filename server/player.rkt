#lang racket

(provide player delay!)

(struct player ([last-request #:mutable]))

(define timer (make-parameter current-inexact-milliseconds))

(define (delay! player)
  (let* ([now ((timer))] 
         [delay (max (- 100.0 (- now (player-last-request player))) 0.0)])
    (set-player-last-request! player (+ now delay))
    (/ delay 1000.0))) 

(module+ test
  (require rackunit)

  (test-case
   "delay"
   (define ((fake-timer value)) value)
   (let ([player (player 0)])
     (parameterize ([timer (fake-timer 1000)])
       (check-equal? (delay! player) 0.0))
     (parameterize ([timer (fake-timer 1010)])
       (check-= (delay! player) 0.09 .001))
     (parameterize ([timer (fake-timer 1105)])
       (check-= (delay! player) 0.095 .001))))

  )