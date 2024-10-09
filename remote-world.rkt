#lang racket

(provide remote-connect remote-draw remote-execs)
(require "bot-info.rkt" "entity.rkt" "location.rkt" "world.rkt")

(define (list->string list)
  (with-output-to-string (λ () (write list))))

(define (strings->string strings)
  (string-append "(" (string-join strings) ")") )

(define (make-info-response success? info)
  (list->string (list success? (bot-info->list info))))

(define (make-response success? entity world)
  (make-info-response success? (bot-info entity (neighbors world entity))))

(define execute-procedures (vector drop-entity! move-entity! take-entity!))

(define (remote-execs world list)

  (define (exec-request request)
    (make-response
     (apply (vector-ref execute-procedures (first request)) (cons world (rest request)))
     (entity-ref world (second request))
     world))

  (strings->string (map exec-request (with-input-from-string list read))))

(define (remote-connect world)
  (strings->string
   (map (λ (bot) (make-response #t bot world))
        (for/list ([i 4])
          (add-entity! world type-bot (location (+ 10 (* i 10)) (+ 10 (* i 10))))))))

(define (remote-draw world)
  (define response '())
  (define (draw-entity symbol x y)
    (set! response (cons (list symbol x y) response)))
  (draw-entities world draw-entity)
  (list->string response))

(module+ test
  (require rackunit)

  (test-case
   "connect creates bots"
   (check-equal?
    (remote-connect (make-world 50))
    "((#t ((101 0 (10 10) #f) ()))\
 (#t ((102 0 (20 20) #f) ()))\
 (#t ((103 0 (30 30) #f) ()))\
 (#t ((104 0 (40 40) #f) ())))"))

  (test-case
   "execute performs commands"
   (let* ([world (make-world 3)]
          [bot (add-entity! world type-bot (location 1 1))])
     (check-equal?
      (remote-execs world "((1 101 1))")
      "((#t ((101 0 (2 1) #f) ((0 3 (3 1) #f)))))"))))
  