#lang racket

(provide connect-remote)
(require net/http-client)
(require "bot-info.rkt" "entity.rkt" "location.rkt" "gen-server.rkt")

(struct remote-server (caller)
  #:methods gen:server
  [(define (add-bot! server location)
     (make-request server (~a "add/" type-bot "/" (location-x location) "/" (location-y location))))

   (define (drop-block! server bot-id direction)
     (make-request server (~a "drop/" bot-id "/" direction)))

   (define (move-bot! server bot-id direction)
     (make-request server (~a "move/" bot-id "/" direction)))

   (define (take-block! server bot-id block-id)
     (make-request server (~a "take/" bot-id "/" block-id)))])

(define (connect-remote host port) (remote-server (remote-call host port)))

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

(define (make-bot-info string)
  (list->bot-info (with-input-from-string string read)))

(define (make-request server path)
  (make-bot-info ((remote-server-caller server) path)))

(module+ test
  (require rackunit "direction.rkt" "remote-world.rkt")

  (define (test-call path)
    (let* ([pieces (string-split path "/")]
           [method (first pieces)]
           [parms (map string->number (rest pieces))]
           [dispatch (make-hash
                      (list (cons "add" remote-add) (cons "move" remote-move)
                            (cons "drop" remote-drop) (cons "take" remote-take)))])
      (apply (hash-ref dispatch method) parms)))

  (test-case
   "add bot remote"
   (let ([server (remote-server test-call)])
     (check-equal?
      (entity-location (bot-info-bot (add-bot! server (location 1 2))))
      (location 1 2))))

  (test-case
   "move bot remote"
   (let* ([server (remote-server test-call)]
         [bot (bot-info-bot (add-bot! server (location 1 2)))])
     (check-equal?
      (entity-location (bot-info-bot (move-bot! server (entity-id bot) direction-east)))
      (location 2 2))))
  
  (test-case
   "take block remote"
   (let* ([server (remote-server test-call)]
         [bot (bot-info-bot (add-bot! server (location 1 2)))]
         [block (bot-info-bot (make-bot-info (remote-add type-block 2 2)))]
         [info (take-block! server (entity-id bot) (entity-id block))])
     (check-equal?
      (entity-id (entity-cargo (bot-info-bot info)))
      (entity-id block))))
  
  (test-case
   "drop block remote"
   (let* ([server (remote-server test-call)]
         [bot (bot-info-bot (add-bot! server (location 1 2)))]
         [block (bot-info-bot (make-bot-info (remote-add type-block 2 2)))]
         [info-1 (take-block! server (entity-id bot) (entity-id block))]
         [info-2 (drop-block! server (entity-id bot) direction-west)])
     (check-false (entity-cargo (bot-info-bot info-2))))))
