#lang racket

(provide make-server connect-server)
(require threading)
(require "bot-info.rkt" "entity.rkt" "location.rkt" "world.rkt" "gen-server.rkt")
(module+ test (require rackunit))

(struct local-server (world)
  #:methods gen:server
  [(define (add-bot! server location)
     (make-response server (add-entity! (local-server-world server) type-bot location)))

   (define (drop-block! server bot-id block-id)
     (make-response server (drop-entity! (local-server-world server) bot-id block-id)))

   (define (move-bot! server bot-id location)
     (make-response server (move-entity! (local-server-world server) bot-id location)))

   (define (take-block! server bot-id block-id)
     (make-response server (take-entity! (local-server-world server) bot-id block-id)))])

(define (make-server size) (local-server (make-world size))) ; used for testing
(define (connect-server world) (local-server world))

(define (make-response server bot)
  (if bot
      (bot-info bot (neighbors (local-server-world server) bot))
      #f))

(module+ test
  (test-case
   "neighbors added to server response"
   (let ([server (make-server 3)])
     (add-entity! (local-server-world server) type-block (location 2 1))
     (let ([neighbor (~> (add-bot! server (location 1 1)) bot-info-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))