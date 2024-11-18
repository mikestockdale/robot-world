#lang racket

(provide (struct-out action) action-bot 
         perform-actions)

(require threading)
(require "shared.rkt" "client/connection.rkt")

(struct action (request-type parameter strategy success? bot))

(define (action-bot-id action) (bot-id (action-bot action)))

(define (perform-actions connection to-do)
  
  (define (perform-procedure action)
    ((action-strategy action) action))

  (define ((copy-action input-action) success? bot)
    (struct-copy action input-action [success? success?] [bot bot]))

  (define (make-request action)
    (request (action-request-type action) (action-bot-id action) (action-parameter action)))
  
  (define (handle-replies replies process-reply-list)
    (map (λ (reply process-reply)
           (process-reply (first reply) (second reply)))
         replies
         process-reply-list))
  
  (let* ([actions (map perform-procedure to-do)]
         [process-replies (map copy-action actions)])
    (handle-replies (connection (map make-request actions)) process-replies)))

(module+ test
  (require rackunit "server/engine.rkt")
  
  (define (go-north input-action)
    (struct-copy action input-action
                 [request-type request-move] [parameter direction-north] [strategy go-east]))
  (define (go-east input-action)
    (struct-copy action input-action
                 [request-type request-move] [parameter direction-east] [strategy go-north]))
  
  (define (simple-actions)
    (let ([bot1 (entity 101 type-bot (location 1 1))]
          [bot2 (entity 102 type-bot (location 0 0))])
      (list
       (action #f #f go-north #f (bot bot1 #f '()))
       (action #f #f go-east #f (bot bot2 #f '())))))

  (define (fake-connection requests)
    (map (λ (command) (list #t command)) requests))
  
  (test-case
   "simple actions are performed"
   (let* ([action-list (perform-actions fake-connection (simple-actions))])
     (check-true (~> action-list first action-success?))
     (check-equal? (~> action-list first action-bot) (request request-move 101 direction-north))
     (check-true (~> action-list second action-success?))
     (check-equal? (~> action-list second action-bot) (request request-move 102 direction-east))))
  
  (test-case
   "simple actions are copied"
   (let* ([action-list
           (perform-actions fake-connection (simple-actions))])
     (check-equal? (~> action-list first action-strategy) go-east)
     (check-equal? (~> action-list second action-strategy) go-north))))
