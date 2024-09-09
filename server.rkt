#lang racket

(provide (struct-out info)
         make-server connect-server move-bot! add-bot! take-block!)
(require threading)
(require "entity.rkt" "location.rkt" "world.rkt")
(module+ test (require rackunit))

(struct server (world))
(struct info (bot neighbors))

(define (make-server size) (server (make-world size))) ; used for testing

(define (connect-server world) (server world))

(define (make-response server bot)
  (if bot
      (info bot (neighbors (server-world server) bot))
      #f))

(define (move-bot! server bot-id location)
  (make-response server (move-entity! (server-world server) bot-id location)))

(define (add-bot! server location)
  (make-response server (add-entity! (server-world server) type-bot location)))

(define (take-block! server bot-id block-id)
  (make-response server (take-entity! (server-world server) bot-id block-id)))

(module+ test
  (test-case
   "neighbors added to server response"
   (let ([server (make-server 3)])
     (add-entity! (server-world server) type-block (location 2 1))
     (let ([neighbor (~> (add-bot! server (location 1 1)) info-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))