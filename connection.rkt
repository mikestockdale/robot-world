#lang racket

(provide connect-local connect-remote send-commands send-draw send-hello)

(require net/http-client)
(require "command.rkt" "shared.rkt")

(define (send-commands connection request-list process-reply-list)
  (let* ([requests (list request-execute-list request-list)]
         [replies (connection requests)])
    (map (λ (reply process-reply)
           (process-reply (first reply) (second reply)))
         replies
         process-reply-list)))

(define (send-draw connection)
  (connection (list request-draw)))

(define (send-hello connection)
  (map (λ (reply) (second reply)) (connection (list request-hello))))

(define (connect-remote host port) (remote-call host port))
(define (connect-local world) (local-call world))

(define (remote-call host port)
  (let-values ([(in out) (tcp-connect host port)])
    (file-stream-buffer-mode in 'none)
    (λ (request-list)
      (write request-list out)
      (flush-output out)
      (read in))))

(define ((local-call world) request-list)
  (define (fake-network item)
    (with-input-from-string
        (with-output-to-string (λ () (write item))) read))
  (fake-network (dispatch-request world (fake-network request-list))))

(module+ test
  (require rackunit threading
           "shared.rkt" "world.rkt")

  (define (process-bot success? bot) bot)

  (test-case
   "list of moves"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [commands
           (list
            (list move-command (entity-id bot) direction-east)
            (list move-command (entity-id bot) direction-south))]
          [processes (list process-bot process-bot)]
          [bots (send-commands connection commands processes)])
     (check-equal? (bot-location (second bots)) (location 2 1))))
  
  (test-case
   "take block remote"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [block (add-entity! world type-block (location 2 2))]
          [bots
           (send-commands connection
                          (list (list take-command (entity-id bot) (entity-id block)))
                          (list process-bot))])
     (check-equal?
      (entity-id (bot-cargo (first bots)))
      (entity-id block))))
  
  (test-case
   "drop block remote"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [block (add-entity! world type-block (location 2 2))]
          [bots-1
           (send-commands connection
                          (list (list take-command (entity-id bot) (entity-id block)))
                          (list process-bot))]
          [bots-2
           (send-commands connection
                          (list (list drop-command (entity-id bot) direction-west))
                          (list process-bot))])
     (check-false (bot-cargo (first bots-2)))))
  
  (test-case
   "neighbors added to server response"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 0 1))])
     (add-entity! world type-block (location 2 1))
     (let ([neighbor
            (~> (send-commands connection
                               (list (list move-command (entity-id bot) direction-east))
                               (list process-bot))
                first bot-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))
