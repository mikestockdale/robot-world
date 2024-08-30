#lang racket

(module+ test (require rackunit))

(struct location (x y) #:transparent)
(struct bot (location))

; server
(define start-location (location 10 10))
(define (add-bot!) start-location)

; client
(define (make-bot) (bot (add-bot!)))

(module+ test
  (test-case
   "bot is created at start location"
   (check-equal? (bot-location (make-bot)) start-location)))
