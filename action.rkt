#lang racket

(provide (struct-out action) action-bot 
         perform-actions)

(require threading)
(require "shared.rkt" "connection.rkt")

(struct action (command parameter strategy success? bot))

(define (action-bot-id action) (bot-id (action-bot action)))

(define (perform-actions connection to-do)
  
  (define (perform-procedure action)
    ((action-strategy action) action))

  (define ((make-action input-action) success? bot)
    (struct-copy action input-action [success? success?] [bot bot]))

  (define (make-command action)
    (command (action-command action) (action-bot-id action) (action-parameter action)))
  
  (let* ([requests (map perform-procedure to-do)]
         [process-replies (map make-action requests)])
    (send-commands connection (map make-command requests) process-replies)))

(module+ test
  (require rackunit "server/engine.rkt")
  
  (define (go-north input-action)
    (struct-copy action input-action
                 [command move-command] [parameter direction-north] [strategy go-east]))
  (define (go-east input-action)
    (struct-copy action input-action
                 [command move-command] [parameter direction-east] [strategy go-north]))
  
  (define (simple-actions)
    (let ([bot1 (entity 101 type-bot (location 1 1))]
          [bot2 (entity 102 type-bot (location 0 0))])
    (list
     (action #f #f go-north #f (bot bot1 #f '()))
     (action #f #f go-east #f (bot bot2 #f '())))))

  (define (fake-connection request)
    (map (λ (command) (list #t command)) (request-commands request)))
  
  (test-case
   "simple actions are performed"
   (let* ([action-list (perform-actions fake-connection (simple-actions))])
     (check-true (~> action-list first action-success?))
     (check-equal? (~> action-list first action-bot) (command move-command 101 direction-north))
     (check-true (~> action-list second action-success?))
     (check-equal? (~> action-list second action-bot) (command move-command 102 direction-east))))
  
  (test-case
   "simple actions are copied"
   (let* ([action-list
           (perform-actions fake-connection (simple-actions))])
     (check-equal? (~> action-list first action-strategy) go-east)
     (check-equal? (~> action-list second action-strategy) go-north))))
