#lang racket

(provide (struct-out actions) perform-actions make-actions)
(require "location.rkt" "world.rkt")
(module+ test (require rackunit))

(struct actions (world list))
(struct action (bot-id procedure))

(define (make-actions world . setups)
  (define (make-action setup)
    (action (bot-id (add-bot! world (first setup))) (second setup)))
  (actions world (map make-action setups)))

(define (perform-actions actions)
  (for-each
   (λ (action)
     ((action-procedure action) (actions-world actions) (action-bot-id action)))
   (actions-list actions)))

(module+ test
  (test-case
   "actions are performed"
   (define (go-north! world bot-id) (move-bot! world bot-id direction-north))
   (define (go-northeast! world bot-id)
     (go-north! world bot-id)
     (move-bot! world bot-id direction-east))
   (define world (make-world 3))
   (perform-actions
    (make-actions world
                  (list (location 1 1) go-northeast!)
                  (list (location 0 0) go-north!)))
   (check-equal? (draw-world world)  #("  O" "O  " "   "))))
     