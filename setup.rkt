#lang racket

(provide setup-blocks setup-bots setup-engine)
(require "shared.rkt" "server/engine.rkt")

(define (setup-engine)
  (make-engine 50))

(define (setup-blocks engine)
  (for ([x 5])
    (for ([y 5])
      (add-entity! engine type-block
                   (location (+ 5 (* x 10)) (+ 5 (* y 10))))))
  #t)

(define (setup-bots engine)
  (for/list ([i 4])
    (add-entity! engine type-bot (location (+ 10 (* i 10)) (+ 10 (* i 10))))))
