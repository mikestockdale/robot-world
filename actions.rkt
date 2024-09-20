#lang racket

(provide (struct-out actions) (struct-out action) action-bot action-bot-id
         perform-actions)

(require threading)
(require "bot-info.rkt" "entity.rkt" "server.rkt")
(module+ test (require rackunit "direction.rkt" "location.rkt"))

(struct actions (server list))
(struct action (info procedure))

(define (action-bot action) (bot-info-bot (action-info action)))
(define (action-bot-id action) (entity-id (action-bot action)))

(define (perform-actions to-do)
  (actions
   (actions-server to-do)
   (map
    (λ (action)
      ((action-procedure action) (actions-server to-do) action))
    (actions-list to-do))))

(module+ test
  (define ((simple-action procedure) server input)
    (let ([entity (bot-info-bot (procedure server (action-bot input)))])
      (struct-copy action input [info (bot-info 50 #t entity #f)])))
  (define (go-north! server bot) (move-bot! server (entity-id bot) direction-north))
  (define (go-northeast! server bot)
    (go-north! server bot)
    (move-bot! server (entity-id bot) direction-east))
  (define (simple-actions server)
    (actions server
             (list
              (action (add-bot! server (location 1 1)) (simple-action go-northeast!))
              (action (add-bot! server (location 0 0)) (simple-action go-north!)))))
  
  (test-case
   "simple actions are performed"
   (let* ([server (make-server 3)]
          [action-list (~> server simple-actions perform-actions actions-list)])
     (check-equal? (~> action-list first action-bot entity-location) (location 2 2))
     (check-equal? (~> action-list second action-bot entity-location) (location 0 1))))

  (test-case
   "simple actions are copied"
   (let* ([server (make-server 4)]
          [action-list (~> server simple-actions perform-actions perform-actions actions-list)])
     (check-equal? (~> action-list first action-bot entity-location) (location 3 3))
     (check-equal? (~> action-list second action-bot entity-location) (location 0 2)))))
