#lang racket

(provide (struct-out step) perform-steps step-id step-location)
(require threading "shared.rkt" "bot.rkt")
(module+ test (require rackunit))

;@title{Step}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/step.rkt" "step.rkt"]}
;A @bold{step} is a single operation requested by a client, to be performed on one bot.
;The step strategy is a function that chooses the next operation, specified by the request type and the parameter.
;The result of the operation is its success status and the state of the bot after the operation.

(struct step (strategy request-type parameter success? bot))

(define (perform-strategy step) ((step-strategy step) step))
(define (step-id step) (bot-id (step-bot step)))
(define (step-location step) (bot-location (step-bot step)))

;To @bold{perform steps}, the strategy functions return two values: a strategy for the next step, and a request to send to the server for this step.
;The requests are sent to the server, and the replies are saved.

(test-case:
 "actions are performed"
 (define (go-north step)
   (values go-east (request request-move (step-id step) direction-north)))
 (define (go-east step)
   (values go-north (request request-move (step-id step) direction-east)))
 (define steps
   (list
    (step go-north #f #f #f (bot 101 #f #f '()))
    (step go-east #f #f #f (bot 102 #f #f '()))))
 (define (fake-connection requests)
   (map (λ (request)
          (reply #t (request-id request) (location 1 1) #f '())) requests))
 (let* ([step-list (perform-steps fake-connection steps)])
   (check-true (~> step-list first step-success?))
   (check-equal? (~> step-list first step-id) 101)
   (check-equal? (~> step-list first step-strategy) go-east)
   (check-true (~> step-list second step-success?))
   (check-equal? (~> step-list second step-id) 102)
   (check-equal? (~> step-list second step-strategy) go-north)))

;The strategy functions are performed for each step in the step list.
;This creates two lists: strategies for the next steps, and requests to send to the server.
;The server connection sends the request list and returns a reply list.
;These three lists: strategies, requests and replies, are merged to create a new step list.

(define (perform-steps connection action-list)
  (define (make-step strategy request reply)
    (step strategy
            (request-type request)
            (request-parameter request)
            (reply-success? reply)
            (make-bot reply)))
  (let-values
      ([(strategies requests)
        (for/lists (l1 l2) ([action action-list]) (perform-strategy action))])
    (map make-step
         strategies
         requests
         (connection requests))))
