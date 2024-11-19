#lang racket

(provide make-interval)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Interval}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/interval.rkt" "interval.rkt"]}
;An interval is the minimum time in between requests from an agent.
;The interval is  measured from the last time a request was processed.

(define (make-interval) (measure-from 0))

;An agent can be active only once every 100 milliseconds.
;The interval calculates the @bold{delay} required until the next request time.

(test-case:
   "delay and next calculated"
   (define (check-values calculate expected-delay expected-next)
     (let-values ([(delay next) (calculate)])
       (check-= delay expected-delay .001)
       (check-= next expected-next .001)))
   (check-values (λ () (delay 0 1000)) 00 1000)
      (check-values (λ () (delay 1000 1000)) 100 1100)
      (check-values (λ () (delay 1000 1050)) 50 1100)
      (check-values (λ () (delay 1000 1100)) 0 1100))

;The delay is calculated as the difference from now to the last active time plus 100 milliseconds.
;The next active time is now plus the delay.

(define interval-milliseconds 100.0)

(define (delay last-active-time now)
  (let ([delay (max (+ last-active-time interval-milliseconds (- now)) 0.0)])
    (values delay (+ now delay))))

;To @bold{measure from} the last active time, the @racket[current-inexact-milliseconds] function is used to calculate the delay.
;The @racket[sleep] function takes a time in seconds, so the delay is divided by 1000.
;The return value is a @racket[measure-from] function to be used for the next interval. 

(define ((measure-from last-active-time))
  (let-values ([(delay next) (delay last-active-time (current-inexact-milliseconds))])
    (sleep (/ delay 1000.0))
    (measure-from next)))
           