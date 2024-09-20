#lang racket

(provide remote-add remote-draw remote-drop remote-move remote-take)
(require "bot-info.rkt" "location.rkt" "world.rkt")

(define (list->string list)
  (with-output-to-string (λ () (write list))))

(define (make-response entity world)
  (make-response/x #t entity world))

(define (make-response/x success? entity world)
  (list->string (bot-info->list (bot-info (world-size world) success? entity (neighbors world entity)))))

(define (remote-add world type x y)
  (make-response (add-entity! world type (location x y)) world))

(define (remote-drop world id direction)
  (make-response/x (drop-entity! world id direction) (entity-ref world id) world))

(define (remote-move world id direction)
  (make-response/x (move-entity! world id direction) (entity-ref world id) world))

(define (remote-take world bot-id block-id)
  (make-response (take-entity! world bot-id block-id) world))

(define (remote-draw world)
  (define response '())
  (define (draw-entity symbol x y)
    (set! response (cons (list symbol x y) response)))
  (draw-entities world draw-entity)
  (list->string response))
