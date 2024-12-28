#lang racket/base

(provide connect-local)
(require racket/list racket/port)
(require "server/dispatcher.rkt")

(define (connect-local engine) (local-call (make-dispatcher) engine))

(define ((local-call dispatcher engine) request-list)
  (define (fake-network item)
    (with-input-from-string
        (with-output-to-string (λ () (write item))) read))
  (fake-network (dispatcher engine (fake-network request-list))))

(module+ test
  (require rackunit threading
           "shared.rkt" "server/engine.rkt" "client/connection.rkt" "client/bot.rkt")
  
  (define (process-bot success? bot) bot)
  
  (define (prepare-connection engine)
    (let ([connection (connect-local engine)])
      (connection request-hello)
      connection))
  
  (define (send-requests connection requests process-reply-list)
    (let ([replies (connection requests)])
      (map (λ (reply process-reply)
             (process-reply (reply-success? reply) (make-bot reply)))
           replies
           process-reply-list)))

  (test-case
   "list of moves"
   (let* ([engine (make-engine 40 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity  engine type-bot (location 1 2))]
          [requests
           (list
            (request request-move (entity-id bot) (location 2 2))
            (request request-move (entity-id bot) (location 2 1)))]
          [processes (list process-bot process-bot)]
          [bots (send-requests connection requests processes)])
     (check-equal? (bot-location (second bots)) (location 2 1))))

  (test-case
   "take block remote"
   (let* ([engine (make-engine 40 50)]
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
   (let* ([engine (make-engine 40 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity engine type-bot (location 1 2))]
          [block (add-entity engine type-block (location 2 2))]
          [bots-1
           (send-requests connection
                          (list (request request-take (entity-id bot) (entity-id block)))
                          (list process-bot))]
          [bots-2
           (send-requests connection
                          (list (request request-drop (entity-id bot) (location 0 2)))
                          (list process-bot))])
     (check-false (bot-cargo (first bots-2)))))
  
  (test-case
   "neighbors added to server response"
   (let* ([engine (make-engine 40 50)]
          [connection (prepare-connection engine)]
          [bot (add-entity engine type-bot (location 0 1))])
     (add-entity engine type-block (location 2 1))
     (let ([neighbor
            (~> (send-requests connection
                               (list (request request-move (entity-id bot) (location 1 1)))
                               (list process-bot))
                first bot-neighbors first)])
       (check-equal? (entity-type (neighbor-entity neighbor)) type-block)
       (check-equal? (neighbor-location neighbor) (location 2 1))))))
