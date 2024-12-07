#lang racket

(provide (struct-out action) perform-actions)
(require threading "shared.rkt" "bot.rkt")
(module+ test (require rackunit))

;@title{Action}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/action.rkt" "action.rkt"]}
;An action is a single operation requested by a client, to be performed on one bot.
;The action strategy is a function that chooses the next operation, specified by the request type and the parameter.
;The result of the operation is its success status and the bot after the operation.

(struct action (strategy request-type parameter success? bot))

;To @bold{perform actions}, the strategy functions set the request types and parameters, and new strategies for subsequent actions.
;The requests are sent to the server, and the replies are saved.

(test-case:
 "actions are performed"
 (define (go-north input-action)
   (struct-copy action input-action [strategy go-east]
                [request-type request-move] [parameter direction-north]))
 (define (go-east input-action)
   (struct-copy action input-action [strategy go-north]
                [request-type request-move] [parameter direction-east]))
 (define actions
   (list
    (action go-north #f #f #f (bot (entity 101 type-bot) #f #f '()))
    (action go-east #f #f #f (bot (entity 102 type-bot) #f #f '()))))
 (define (fake-connection requests)
   (map (λ (request)
          (reply #t (entity (request-id request) type-bot) (location 1 1) #f '())) requests))
 (let* ([action-list (perform-actions fake-connection actions)])
   (check-true (~> action-list first action-success?))
   (check-equal? (~> action-list first action-bot bot-entity entity-id) 101)
   (check-equal? (~> action-list first action-strategy) go-east)
   (check-true (~> action-list second action-success?))
   (check-equal? (~> action-list second action-bot bot-entity entity-id) 102)
   (check-equal? (~> action-list second action-strategy) go-north)))

;The actions are performed in these steps:
;@itemlist[
;@item{the strategies are performed}
;@item{the requests are created}
;@item{the requests are sent to the server}
;@item{the actions are copied with the reply information updated}] 

(define (perform-actions connection action-list)
  (define (perform-strategy action)
    ((action-strategy action) action))
  (define (make-request action)
    (request
     (action-request-type action)
     (bot-id (action-bot action))
     (action-parameter action)))
  (define (copy-action reply input-action)
    (struct-copy action input-action
                 [success? (reply-success? reply)] [bot (make-bot reply)]))
  (let ([actions (map perform-strategy action-list)])
    (map copy-action 
         (connection (map make-request actions))
         actions)))
