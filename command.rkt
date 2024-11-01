#lang racket

(require "shared.rkt" "setup.rkt" "world.rkt")

(provide drop-command move-command take-command
         dispatch-request execute-command-list execute-draw execute-hello)

(define (dispatch-request world request-list)
  (let* ([method (first request-list)]
         [parms (cons world (rest request-list))]
         [dispatch (make-hash
                    (list (cons "execs" execute-command-list)
                          (cons "draw" execute-draw)
                          (cons "hello" execute-hello)))])
    (apply (hash-ref dispatch method) parms)))

(define drop-command 0)
(define move-command 1)
(define take-command 2)

(define command-procedures (vector drop-entity! move-entity! take-entity!))

(define (make-response-list success? entity-id world)
  (list success? (make-bot world entity-id)))

(define (execute-command-list world list)

  (define (exec-request request)
    (make-response-list
     (apply (vector-ref command-procedures (first request)) (cons world (rest request)))
     (second request)
     world))

  (map exec-request list))

(define (execute-draw world)
  (define response '())
  (define (draw-entity symbol x y)
    (set! response (cons (list symbol x y) response)))
  (draw-entities world draw-entity)
  response)

(define (execute-hello world)
  (map (λ (bot) (make-response-list #t (entity-id bot) world))
       (setup-bots world)))

(module+ test
  (require rackunit)

  (test-case
   "connect creates bots"
   (check-equal?
    (execute-hello (make-world 50))
    '((#t #s(bot #s(entity 101 0 #s(location 10 10)) #f ()))
      (#t #s(bot #s(entity 102 0 #s(location 20 20)) #f ()))
      (#t #s(bot #s(entity 103 0 #s(location 30 30)) #f ()))
      (#t #s(bot #s(entity 104 0 #s(location 40 40)) #f ())))))

  (test-case
   "execute performs commands"
   (let* ([world (make-world 3)]
          [bot (add-entity! world type-bot (location 1 1))])
     (check-equal?
      (execute-command-list world '((1 101 1)))
      '((#t
         #s(bot
            #s(entity 101 0 #s(location 2 1)) #f
            (#s(entity 0 2 #s(location 3 1))))))))))
