#lang racket

(provide connect-local connect-remote make-server
         add-bot! execute-list!)
(require net/http-client)
(require "bot-info.rkt" "entity.rkt" "location.rkt" "remote-world.rkt" "world.rkt")

(struct server (caller))

(define (add-bot! server location)
  (make-request server (~a "add/" type-bot "/" (location-x location) "/" (location-y location))))

(define (execute-list! server list)
  (let* ([path (string-append "execs/" (with-output-to-string (λ () (write list))))]
    [reply ((server-caller server) path)])
    (map list->bot-info (with-input-from-string reply read))))

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
                    (list (cons "add" remote-add) 
                          (cons "exec" remote-exec)
                          (cons "execs" remote-execs)))])
   (apply (hash-ref dispatch method) parms)))

(define (make-bot-info string)
  (list->bot-info (with-input-from-string string read)))

(define (make-request server path)
  (make-bot-info ((server-caller server) path)))

(module+ test
  (require rackunit threading "direction.rkt" "execute.rkt" "world.rkt")

  (test-case
   "add bot remote"
   (let ([server (make-server 3)])
     (check-equal?
      (entity-location (bot-info-bot (add-bot! server (location 1 2))))
      (location 1 2))))

  (test-case
   "execute list"
   (let* ([server (make-server 3)]
          [bot (bot-info-bot (add-bot! server (location 1 2)))]
          [commands
           (list
            (list execute-move (entity-id bot) direction-east)
            (list execute-move (entity-id bot) direction-south))]
          [infos (execute-list! server commands)])
     (check-equal? (entity-location (bot-info-bot (second infos))) (location 2 1))))
  
  (test-case
   "take block remote"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [bot (bot-info-bot (add-bot! server (location 1 2)))]
          [block (bot-info-bot (make-bot-info (remote-add world type-block 2 2)))]
          [infos
           (execute-list! server (list (list execute-take (entity-id bot) (entity-id block))))])
     (check-equal?
      (entity-id (entity-cargo (bot-info-bot (first infos))))
      (entity-id block))))
  
  (test-case
   "drop block remote"
   (let* ([world (make-world 3)]
          [server (connect-local world)]
          [bot (bot-info-bot (add-bot! server (location 1 2)))]
          [block (bot-info-bot (make-bot-info (remote-add world type-block 2 2)))]
          [infos-1
           (execute-list! server (list (list execute-take (entity-id bot) (entity-id block))))]
          [infos-2
           (execute-list! server (list (list execute-drop (entity-id bot) direction-west)))])
     (check-false (entity-cargo (bot-info-bot (first infos-2))))))
  
  (test-case
   "neighbors added to server response"
   (let* ([world (make-world 3)]
          [server (connect-local world)])
     (add-entity! world type-block (location 2 1))
     (let ([neighbor (~> (add-bot! server (location 1 1)) bot-info-neighbors first)])
       (check-equal? (entity-type neighbor) type-block)
       (check-equal? (entity-location neighbor) (location 2 1))))))
