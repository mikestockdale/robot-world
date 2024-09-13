#lang racket

(provide remote-add remote-move)
(require "bot-info.rkt" "location.rkt" "world.rkt")

(define world (make-world 50))

(define (make-response entity)
  (with-output-to-string (λ () (write (bot-info->list (bot-info entity (neighbors world entity)))))))

(define (remote-add type x y)
  (make-response (add-entity! world type (location x y))))

(define (remote-move id direction)
  (make-response (move-entity! world id direction)))
