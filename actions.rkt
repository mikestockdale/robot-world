#lang racket

(provide (struct-out actions) (struct-out action)
         perform-actions make-actions simple-action)

(require threading)
(require "location.rkt" "world.rkt")
(module+ test (require rackunit))

(struct actions (world list))
(struct action (bot-id procedure))

(define (make-actions world . setups)
  (define (make-action setup)
    (action (bot-id (add-bot! world (first setup))) (second setup)))
  (actions world (map make-action setups)))

(define (perform-actions to-do)
  (actions
   (actions-world to-do)
   (map
    (λ (action)
      ((action-procedure action) (actions-world to-do) action))
    (actions-list to-do))))

(define ((simple-action procedure) world action) (procedure world (action-bot-id action)) action)

(module+ test
  (define (go-north! world bot-id) (move-bot! world bot-id direction-north))
  (define (go-northeast! world bot-id)
    (go-north! world bot-id)
    (move-bot! world bot-id direction-east))
  (define (simple-actions world)
    (make-actions world
                  (list (location 1 1) (simple-action go-northeast!))
                  (list (location 0 0) (simple-action go-north!))))
  
  (test-case
   "simple actions are performed"
   (define world (make-world 3))
   (perform-actions (simple-actions world))
   (check-equal? (draw-world world)  #("  O" "O  " "   ")))

  (test-case
   "simple actions are copied"
   (define world (make-world 4))
   (~>
    (simple-actions world)
    perform-actions
    perform-actions)
   (check-equal? (draw-world world)  #("   O" "O   " "    " "    "))))
