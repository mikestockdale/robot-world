#lang racket

(provide (struct-out action) perform-actions move-failed?)
(require threading "shared.rkt" "bot.rkt")
(module+ test (require rackunit))

;@title{Action}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/action.rkt" "action.rkt"]}
;An action is a single operation requested by a client, to be performed on one bot.
;The action strategy is a function that chooses the next operation, specified by the request type and the parameter.
;The result of the operation is its success status and the bot after the operation.

(struct action (strategy request-type parameter success? bot))

(define (move-failed? action)
  (and (equal? (action-request-type action) request-move)
                  (not (action-success? action))))

;To @bold{perform actions}, the strategy functions set the request types and parameters, and new strategies for subsequent actions.
;The requests are sent to the server, and the replies are saved.

(test-case:
 "actions are performed"
 (define (go-north input-action)
   (values go-east (request request-move (bot-id (action-bot input-action)) direction-north)))
 (define (go-east input-action)
   (values go-north (request request-move (bot-id (action-bot input-action)) direction-east)))
 (define actions
   (list
    (action go-north #f #f #f (bot 101 #f #f '()))
    (action go-east #f #f #f (bot 102 #f #f '()))))
 (define (fake-connection requests)
   (map (λ (request)
          (reply #t (entity (request-id request) type-bot) (location 1 1) #f '())) requests))
 (let* ([action-list (perform-actions fake-connection actions)])
   (check-true (~> action-list first action-success?))
   (check-equal? (~> action-list first action-bot bot-id) 101)
   (check-equal? (~> action-list first action-strategy) go-east)
   (check-true (~> action-list second action-success?))
   (check-equal? (~> action-list second action-bot bot-id) 102)
   (check-equal? (~> action-list second action-strategy) go-north)))

;The actions are performed in these steps:
;@itemlist[
;@item{the strategies are performed}
;@item{the requests are created}
;@item{the requests are sent to the server}
;@item{the actions are copied with the reply information updated}] 

(define (perform-actions connection action-list)
  (define (make-action strategy request reply)
    (action strategy
            (request-type request)
            (request-parameter request)
            (reply-success? reply)
            (make-bot reply)))
  (let-values
      ([(strategies requests)
        (for/lists (l1 l2) ([action action-list]) ((action-strategy action) action))])
    (map make-action
         strategies
         requests
         (connection requests))))
