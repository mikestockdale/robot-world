#lang racket

(provide make-interval)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Interval}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/interval.rkt" "interval.rkt"]}
;An interval sets the minimum time in between requests from an agent.
;The interval is  measured from the last time a request was processed.

(struct interval ([last-active-time #:mutable]))
(define (make-interval) (measure (interval 0)))

;An agent can be active only once every 100 milliseconds.
;The interval calculates the @bold{delay} required until the next request time.

(test-case:
 "delay calculated"
 (check-= (delay 0 1000) 0 .001)
 (check-= (delay 1000 1000) .1 .001)
 (check-= (delay 1000 1050) .05 .001)
 (check-= (delay 1000 1100) 0 .001))

;The delay is calculated as the difference from now to the last active time plus 100 milliseconds.
;The return value is in seconds.

(define interval-milliseconds 100.0)

(define (delay last-active-time now)
  (/ (max (+ last-active-time interval-milliseconds (- now)) 0.0) 1000.0))

;The interval uses the @racket[current-inexact-milliseconds] function to @bold{measure} the delay from the last active time.
;The last active time is updated. 

(define ((measure interval))
  (sleep (delay (interval-last-active-time interval) (current-inexact-milliseconds)))
  (set-interval-last-active-time! interval (current-inexact-milliseconds)))
