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

  (define (make-request action)
    (list (action-command action) (action-bot-id action) (action-parameter action)))
  
  (let* ([requests (map perform-procedure to-do)]
         [process-replies (map make-action requests)])
    (send-commands connection (map make-request requests) process-replies)))

(module+ test
  (require rackunit "command.rkt" "world.rkt")
  
  (define (go-north input-action)
    (struct-copy action input-action
                 [command move-command] [parameter direction-north] [strategy go-east]))
  (define (go-east input-action)
    (struct-copy action input-action
                 [command move-command] [parameter direction-east] [strategy go-north]))
  
  (define (simple-actions world)
    (let ([bot1 (add-entity! world type-bot (location 1 1))]
          [bot2 (add-entity! world type-bot (location 0 0))])
    (list
     (action #f #f go-north #f (bot bot1 #f '()))
     (action #f #f go-east #f (bot bot2 #f '())))))
  
  (test-case
   "simple actions are performed"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [action-list (perform-actions server (simple-actions world))])
     (check-true (~> action-list first action-success?))
     (check-equal? (~> action-list first action-bot bot-location) (location 1 2))
     (check-true (~> action-list second action-success?))
     (check-equal? (~> action-list second action-bot bot-location) (location 1 0))))
  
  (test-case
   "simple actions are copied"
   (let* ([world (make-world 4)]
          [server (connect-local world)]
          [action-list
           (perform-actions server (perform-actions server (simple-actions world)))])
     (check-equal? (~> action-list first action-bot bot-location) (location 2 2))
     (check-equal? (~> action-list second action-bot bot-location) (location 1 1)))))
