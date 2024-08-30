#lang racket

(module+ test (require rackunit))

(struct location (x y) #:transparent)
(struct bot (id location))

; server
(define start-location (location 10 10))
(define new-id 101)
(define (add-bot!) (bot new-id start-location))

; client
(define (make-bot) (add-bot!))

(module+ test
  (test-case
   "bot is created at start location"
   (check-equal? (bot-location (make-bot)) start-location))
  (test-case
   "bot is created with new id"
   (check-equal? (bot-id (make-bot)) new-id)))
