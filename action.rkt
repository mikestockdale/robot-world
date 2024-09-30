#lang racket

(provide (struct-out action) action-bot action-bot-id
         perform-actions)

(require threading)
(require "bot-info.rkt" "entity.rkt" "server.rkt")

(struct action (execute parameter procedure info))

(define (action-bot action) (bot-info-bot (action-info action)))
(define (action-bot-id action) (entity-id (action-bot action)))

(define (perform-actions server to-do)
  
  (define (prepare-request action)
    (let-values ([(execute parameter procedure) ((action-procedure action) action)])
      (list procedure execute (action-bot-id action) parameter)))
  
  (let* ([requests (map prepare-request to-do)]
         [bot-infos (execute-list! server (map rest requests))])
          (map (λ (info request)
                 (action (second request)(fourth request)(first request) info))
               bot-infos requests)))

(module+ test
  (require rackunit "direction.rkt" "execute.rkt" "location.rkt")
  
  (define (go-north! action)
    (values execute-move direction-north go-east!))
  (define (go-east! action)
    (values execute-move direction-east go-north!))
  (define (simple-actions server)
    (list
     (action #f #f go-north! (add-bot! server (location 1 1)))
     (action #f #f go-east! (add-bot! server (location 0 0)))))
  
  (test-case
   "simple actions are performed"
   (let* ([server (make-server 3)]
          [action-list (perform-actions server (simple-actions server))])
     (check-equal? (~> action-list first action-bot entity-location) (location 1 2))
     (check-equal? (~> action-list second action-bot entity-location) (location 1 0))))
  
  (test-case
   "simple actions are copied"
   (let* ([server (make-server 4)]
          [action-list
           (perform-actions server (perform-actions server (simple-actions server)))])
     (check-equal? (~> action-list first action-bot entity-location) (location 2 2))
     (check-equal? (~> action-list second action-bot entity-location) (location 1 1)))))
