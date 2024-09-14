#lang racket

(provide remote-add remote-drop remote-move remote-take)
(require "bot-info.rkt" "location.rkt" "world.rkt")

(define world (make-world 50))

(define (make-response entity)
  (with-output-to-string (λ () (write (bot-info->list (bot-info entity (neighbors world entity)))))))

(define (remote-add type x y)
  (make-response (add-entity! world type (location x y))))

(define (remote-drop id direction)
  (make-response (drop-entity! world id direction)))

(define (remote-move id direction)
  (make-response (move-entity! world id direction)))

(define (remote-take bot-id block-id)
  (make-response (take-entity! world bot-id block-id)))
