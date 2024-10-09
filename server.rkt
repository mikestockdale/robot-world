#lang racket

(provide connect-local connect-remote make-server
         execute-list hello)

(require net/http-client)
(require "bot-info.rkt" "entity.rkt" "location.rkt" "remote-world.rkt" "world.rkt")

(struct server (caller))

(define (execute-list server request-list process-reply-list)
  (let* ([path (string-append "execs/" (with-output-to-string (λ () (write request-list))))]
         [replies (with-input-from-string ((server-caller server) path) read)])
    (map (λ (reply process-reply)
           (process-reply (first reply) (list->bot-info (second reply))))
         replies
         process-reply-list)))

(define (hello server)
  (let ([replies (with-input-from-string ((server-caller server) "hello") read)])
    (map (λ (reply) (list->bot-info (second reply)))
         replies)))

(define (connect-remote host port) (server (remote-call host port)))
(define (connect-local world) (server (local-call world)))

(define (make-server size) (connect-local (make-world size))) ; for testing

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
                    (list (cons "execs" remote-execs)
                          (cons "hello" remote-connect)))])
    (apply (hash-ref dispatch method) parms)))

(define (make-bot-info string)
  (let ([result (with-input-from-string string read)])
    (list->bot-info (second result))))

(define (make-request server path)
  (make-bot-info ((server-caller server) path)))

(module+ test
  (require rackunit threading "direction.rkt" "execute.rkt" "world.rkt")

  (define (process-info success? info) info)

  (test-case
   "execute list"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [commands
           (list
            (list execute-move (entity-id bot) direction-east)
            (list execute-move (entity-id bot) direction-south))]
          [processes (list process-info process-info)]
          [infos (execute-list server commands processes)])
     (check-equal? (entity-location (bot-info-bot (second infos))) (location 2 1))))
  
  (test-case
   "take block remote"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [block (add-entity! world type-block (location 2 2))]
          [infos
           (execute-list server
                         (list (list execute-take (entity-id bot) (entity-id block)))
                         (list process-info))])
     (check-equal?
      (entity-id (entity-cargo (bot-info-bot (first infos))))
      (entity-id block))))
  
  (test-case
   "drop block remote"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [bot (add-entity!  world type-bot (location 1 2))]
          [block (add-entity! world type-block (location 2 2))]
          [infos-1
           (execute-list server
                         (list (list execute-take (entity-id bot) (entity-id block)))
                         (list process-info))]
          [infos-2
           (execute-list server
                         (list (list execute-drop (entity-id bot) direction-west))
                         (list process-info))])
     (check-false (entity-cargo (bot-info-bot (first infos-2))))))
  
  (test-case
   "neighbors added to server response"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [bot (add-entity!  world type-bot (location 0 1))])
     (add-entity! world type-block (location 2 1))
     (let ([neighbor (~> (execute-list server
                                       (list (list execute-move (entity-id bot) direction-east))
                                       (list process-info))
                                       first bot-info-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))
