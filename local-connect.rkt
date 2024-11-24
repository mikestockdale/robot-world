#lang racket

(provide connect-local)
(require "server/dispatcher.rkt")

(define (connect-local engine) (local-call (make-dispatcher engine)))

(define ((local-call dispatcher) request-list)
  (define (fake-network item)
    (with-input-from-string
        (with-output-to-string (λ () (write item))) read))
  (fake-network (dispatch-request dispatcher (fake-network request-list))))

(module+ test
  (require rackunit threading
           "shared.rkt" "server/engine.rkt" "client/connection.rkt")
  
  (define (process-bot success? bot) bot)
  
  (define (prepare-connection engine)
    (let ([connection (connect-local engine)])
      (connection request-hello)
      connection))
  
  (define (send-requests connection requests process-reply-list)
    (let ([replies (connection requests)])
      (map (λ (reply process-reply)
             (process-reply (reply-success? reply) (reply-bot reply)))
           replies
           process-reply-list)))

  (test-case
   "list of moves"
   (let* ([engine (make-engine 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity  engine type-bot (location 1 2))]
          [requests
           (list
            (request request-move (entity-id bot) direction-east)
            (request request-move (entity-id bot) direction-south))]
          [processes (list process-bot process-bot)]
          [bots (send-requests connection requests processes)])
     (check-equal? (bot-location (second bots)) (location 2 1))))

  (test-case
   "take block remote"
   (let* ([engine (make-engine 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity engine type-bot (location 1 2))]
          [block (add-entity engine type-block (location 2 2))]
          [bots
           (send-requests connection
                          (list (request request-take (entity-id bot) (entity-id block)))
                          (list process-bot))])
     (check-equal?
      (entity-id (bot-cargo (first bots)))
      (entity-id block))))
  
  (test-case
   "drop block remote"
   (let* ([engine (make-engine 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity engine type-bot (location 1 2))]
          [block (add-entity engine type-block (location 2 2))]
          [bots-1
           (send-requests connection
                          (list (request request-take (entity-id bot) (entity-id block)))
                          (list process-bot))]
          [bots-2
           (send-requests connection
                          (list (request request-drop (entity-id bot) direction-west))
                          (list process-bot))])
     (check-false (bot-cargo (first bots-2)))))
  
  (test-case
   "neighbors added to server response"
   (let* ([engine (make-engine 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity engine type-bot (location 0 1))])
     (add-entity engine type-block (location 2 1))
     (let ([neighbor
            (~> (send-requests connection
                               (list (request request-move (entity-id bot) direction-east))
                               (list process-bot))
                first bot-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))
