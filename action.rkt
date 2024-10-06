#lang racket

(provide (struct-out action) action-bot action-bot-id
         perform-actions)

(require threading)
(require "bot-info.rkt" "entity.rkt" "server.rkt")

(struct action (execute parameter procedure success? info))

(define (action-bot action) (bot-info-bot (action-info action)))
(define (action-bot-id action) (entity-id (action-bot action)))

(define (perform-actions server to-do)
  
  (define (perform-procedure action)
    ((action-procedure action) action))

  (define ((make-action input-action) success? info)
    (struct-copy action input-action [success? success?] [info info]))

  (define (make-request action)
    (list (action-execute action) (action-bot-id action) (action-parameter action)))
  
  (let* ([requests (map perform-procedure to-do)]
         [process-replies (map make-action requests)])
    (execute-list server (map make-request requests) process-replies)))

(module+ test
  (require rackunit "direction.rkt" "execute.rkt" "location.rkt")
  
  (define (go-north input-action)
    (struct-copy action input-action
                 [execute execute-move] [parameter direction-north] [procedure go-east]))
  (define (go-east input-action)
    (struct-copy action input-action
                 [execute execute-move] [parameter direction-east] [procedure go-north]))
  
  (define (simple-actions server)
    (list
     (action #f #f go-north #f (add-bot! server (location 1 1)))
     (action #f #f go-east #f (add-bot! server (location 0 0)))))
  
  (test-case
   "simple actions are performed"
   (let* ([server (make-server 3)]
          [action-list (perform-actions server (simple-actions server))])
     (check-true (~> action-list first action-success?))
     (check-equal? (~> action-list first action-bot entity-location) (location 1 2))
     (check-true (~> action-list second action-success?))
     (check-equal? (~> action-list second action-bot entity-location) (location 1 0))))
  
  (test-case
   "simple actions are copied"
   (let* ([server (make-server 4)]
          [action-list
           (perform-actions server (perform-actions server (simple-actions server)))])
     (check-equal? (~> action-list first action-bot entity-location) (location 2 2))
     (check-equal? (~> action-list second action-bot entity-location) (location 1 1)))))
