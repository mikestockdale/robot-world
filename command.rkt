#lang racket

(require "bot-info.rkt" "setup.rkt" "world.rkt")

(provide drop-command move-command take-command
         execute-command-list execute-draw execute-hello)

(define drop-command 0)
(define move-command 1)
(define take-command 2)

(define command-procedures (vector drop-entity! move-entity! take-entity!))

(define (strings->string strings)
  (string-append "(" (string-join strings) ")") )

(define (write-list->string list)
  (with-output-to-string (λ () (write list))))

(define (make-response success? entity world)
  (write-list->string
   (list success?
         (bot-info->list (bot-info entity (neighbors world entity))))))

(define (execute-command-list world list)

  (define (exec-request request)
    (make-response
     (apply (vector-ref command-procedures (first request)) (cons world (rest request)))
     (entity-ref world (second request))
     world))

  (strings->string (map exec-request (with-input-from-string list read))))

(define (execute-draw world)
  (define response '())
  (define (draw-entity symbol x y)
    (set! response (cons (list symbol x y) response)))
  (draw-entities world draw-entity)
  (write-list->string response))

(define (execute-hello world)
  (strings->string
   (map (λ (bot) (make-response #t bot world))
        (setup-bots world))))

(module+ test
  (require rackunit "entity.rkt" "location.rkt" )

  (test-case
   "connect creates bots"
   (check-equal?
    (execute-hello (make-world 50))
    "((#t ((101 0 (10 10) #f) ()))\
 (#t ((102 0 (20 20) #f) ()))\
 (#t ((103 0 (30 30) #f) ()))\
 (#t ((104 0 (40 40) #f) ())))"))

  (test-case
   "execute performs commands"
   (let* ([world (make-world 3)]
          [bot (add-entity! world type-bot (location 1 1))])
     (check-equal?
      (execute-command-list world "((1 101 1))")
      "((#t ((101 0 (2 1) #f) ((0 3 (3 1) #f)))))"))))
