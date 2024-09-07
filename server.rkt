#lang racket

(provide make-server connect-server move-bot! add-bot!)
(require "entity.rkt" "world.rkt")

(struct server (world))

(define (make-server size) (server (make-world size))) ; used for testing

(define (connect-server world) (server world))

(define (move-bot! server bot-id location) (move-entity! (server-world server) bot-id location))

(define (add-bot! server location) (add-entity! (server-world server) type-bot location))
