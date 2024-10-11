#lang racket

(provide connect-local connect-remote send-commands send-draw send-hello)

(require net/http-client)
(require "bot-info.rkt" "command.rkt")

(define (send-commands connection request-list process-reply-list)
  (let* ([path (string-append "execs/" (with-output-to-string (λ () (write request-list))))]
         [replies (with-input-from-string (connection path) read)])
    (map (λ (reply process-reply)
           (process-reply (first reply) (list->bot-info (second reply))))
         replies
         process-reply-list)))

(define (send-draw connection)
  (with-input-from-string (connection "draw") read))

(define (send-hello connection)
  (let ([replies (with-input-from-string (connection "hello") read)])
    (map (λ (reply) (list->bot-info (second reply)))
         replies)))

(define (connect-remote host port) (remote-call host port))
(define (connect-local world) (local-call world))

(define ((remote-call host port) path)
  (define-values (status headers in)
    (http-sendrecv host
                   path
                   #:port port
                   #:version "1.1"
                   #:method "GET"
                   #:data ""))
  (let ([return (port->string in)])
    (close-input-port in)
    return))

(define ((local-call world) path)
  (let* ([pieces (string-split path "/")]
         [method (first pieces)]
         [parms
          (if (= (length pieces) 2)
              (cons world (rest pieces))
              (cons world (map string->number (rest pieces))))]
         [dispatch (make-hash
                    (list (cons "execs" execute-command-list)
                          (cons "draw" execute-draw)
                          (cons "hello" execute-hello)))])
    (apply (hash-ref dispatch method) parms)))

(module+ test
  (require rackunit threading
           "direction.rkt" "entity.rkt" "location.rkt" "world.rkt")

  (define (process-info success? info) info)

  (test-case
   "list of moves"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [commands
           (list
            (list move-command (entity-id bot) direction-east)
            (list move-command (entity-id bot) direction-south))]
          [processes (list process-info process-info)]
          [infos (send-commands connection commands processes)])
     (check-equal? (entity-location (bot-info-bot (second infos))) (location 2 1))))
  
  (test-case
   "take block remote"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [block (add-entity! world type-block (location 2 2))]
          [infos
           (send-commands connection
                          (list (list take-command (entity-id bot) (entity-id block)))
                          (list process-info))])
     (check-equal?
      (entity-id (entity-cargo (bot-info-bot (first infos))))
      (entity-id block))))
  
  (test-case
   "drop block remote"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [block (add-entity! world type-block (location 2 2))]
          [infos-1
           (send-commands connection
                          (list (list take-command (entity-id bot) (entity-id block)))
                          (list process-info))]
          [infos-2
           (send-commands connection
                          (list (list drop-command (entity-id bot) direction-west))
                          (list process-info))])
     (check-false (entity-cargo (bot-info-bot (first infos-2))))))
  
  (test-case
   "neighbors added to server response"
   (let* ([world (make-world 3)]
          [connection (connect-local world)]
          [bot (add-entity!  world type-bot (location 0 1))])
     (add-entity! world type-block (location 2 1))
     (let ([neighbor
            (~> (send-commands connection
                               (list (list move-command (entity-id bot) direction-east))
                               (list process-info))
                first bot-info-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))
