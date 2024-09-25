#lang racket

(provide (struct-out actions) (struct-out action) action-bot action-bot-id
         perform-actions)

(require threading)
(require "bot-info.rkt" "entity.rkt" "server.rkt")

(struct actions (server list))
(struct action (execute parameter procedure info))

(define (action-bot action) (bot-info-bot (action-info action)))
(define (action-bot-id action) (entity-id (action-bot action)))

(define (perform-actions to-do)
  
  (define (perform-action input)
    (let-values ([(execute parameter procedure) ((action-procedure input) input)])
      (action execute parameter procedure
               (execute! (actions-server to-do)
                         (list execute (~> input action-info bot-info-bot entity-id) parameter)))))
    
  (struct-copy
   actions to-do
   [list (map perform-action (actions-list to-do))]))

(module+ test
  (require rackunit "direction.rkt" "execute.rkt" "location.rkt")
  
  (define (go-north! action)
    (values execute-move direction-north go-east!))
  (define (go-east! action)
    (values execute-move direction-east go-north!))
  (define (simple-actions server)
    (actions server
             (list
              (action #f #f go-north! (add-bot! server (location 1 1)))
              (action #f #f go-east! (add-bot! server (location 0 0))))))
  
  (test-case
   "simple actions are performed"
   (let* ([server (make-server 3)]
          [action-list (~> server simple-actions perform-actions actions-list)])
     (check-equal? (~> action-list first action-bot entity-location) (location 1 2))
     (check-equal? (~> action-list second action-bot entity-location) (location 1 0))))
  
  (test-case
   "simple actions are copied"
   (let* ([server (make-server 4)]
          [action-list (~> server simple-actions perform-actions perform-actions actions-list)])
     (check-equal? (~> action-list first action-bot entity-location) (location 2 2))
     (check-equal? (~> action-list second action-bot entity-location) (location 1 1)))))
