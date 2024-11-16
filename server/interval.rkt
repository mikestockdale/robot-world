#lang racket

(provide make-interval delay)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Interval}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/interval.rkt" "interval.rkt"]}
;An interval is the minimum time in between requests from an agent.
;The interval tracks the last time a request was processed.

(struct interval ([last-active-time #:mutable]))
(define (make-interval) (interval 0))

;An agent can be active only once every 100 milliseconds.
;The interval calculates the @bold{delay} required until the next request time.
;The system function @racket[current-inexact-milliseconds] is used as a timer.
;We can override this with a fake timer in our tests.

(define interval-milliseconds (make-parameter 100.0))
(define timer (make-parameter current-inexact-milliseconds))

(test-case:
 "delay is calculated"
 (define ((fake-timer value)) value)
 (let ([interval (make-interval)])
   (parameterize ([timer (fake-timer 1000)])
     (check-equal? (delay interval) 0.0))
   (parameterize ([timer (fake-timer 1010)])
     (check-= (delay interval) 0.09 .001))
   (parameterize ([timer (fake-timer 1105)])
     (check-= (delay interval) 0.095 .001))))

;The delay is calculated as the difference from now to the last active time plus 100 milliseconds.
;The last active time is updated to now plus the delay.
;The return value is the delay in seconds.

(define (delay interval)
  (let* ([now ((timer))] 
         [delay
           (max
            (+ (interval-last-active-time interval) (interval-milliseconds) (- now))
            0.0)])
    (set-interval-last-active-time! interval (+ now delay))
    (/ delay 1000.0)))
