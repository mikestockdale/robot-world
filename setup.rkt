#lang racket

(provide setup-blocks setup-bots setup-world)
(require "shared.rkt" "world.rkt")

(define (setup-world)
  (make-world 50))

(define (setup-blocks world)
  (for ([x 5])
    (for ([y 5])
      (add-entity! world type-block
                   (location (+ 5 (* x 10)) (+ 5 (* y 10))))))
  #t)

(define (setup-bots world)
  (for/list ([i 4])
    (add-entity! world type-bot (location (+ 10 (* i 10)) (+ 10 (* i 10))))))
