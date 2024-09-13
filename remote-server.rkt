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

(define (make-request server path)
  (list->bot-info (with-input-from-string ((remote-server-caller server) server path) read)))

(module+ test
  (require rackunit "remote-world.rkt")

  (define (test-call server path)
    (let* ([pieces (string-split path "/")]
           [method (first pieces)]
           [parms (map string->number (rest pieces))]
           [dispatch (make-hash (list (cons "add" remote-add) (cons "move" remote-move)))])
      (apply (hash-ref dispatch method) parms)))

  (test-case
   "add bot remote"
   (let ([server (remote-server test-call)])
     (check-equal?
      (entity-location (bot-info-bot (add-bot! server (location 1 2))))
      (location 1 2)))))
