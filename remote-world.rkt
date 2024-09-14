#lang racket

(provide remote-add remote-draw remote-drop remote-move remote-take)
(require "bot-info.rkt" "location.rkt" "world.rkt")

(define world (make-world 50))

(define (list->string list)
  (with-output-to-string (λ () (write list))))

(define (make-response entity)
  (list->string (bot-info->list (bot-info entity (neighbors world entity)))))

(define (remote-add type x y)
  (make-response (add-entity! world type (location x y))))

(define (remote-drop id direction)
  (make-response (drop-entity! world id direction)))

(define (remote-move id direction)
  (make-response (move-entity! world id direction)))

(define (remote-take bot-id block-id)
  (make-response (take-entity! world bot-id block-id)))

(define (remote-draw)
  (define response '())
  (define (draw-entity symbol x y)
    (set! response (cons (list symbol x y) response)))
  (draw-entities world draw-entity)
  (list->string response))
