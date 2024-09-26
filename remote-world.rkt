#lang racket

(provide remote-add remote-draw remote-exec remote-execs)
(require "bot-info.rkt" "entity.rkt" "location.rkt" "world.rkt")

(define null-entity (make-entity 0 0 (location 0 0)))

(define (list->string list)
  (with-output-to-string (λ () (write list))))

(define (make-info-response info)
  (list->string (bot-info->list info)))

(define (make-response success? entity world)
  (make-info-response (bot-info (world-size world) success? entity (neighbors world entity))))

(define (remote-add world type x y)
  (let ([new-entity (add-entity! world type (location x y))])
    (if new-entity
        (make-response #t new-entity world)
        (make-info-response (bot-info (world-size world) #f null-entity '())))))

(define execute-procedures (vector drop-entity! move-entity! take-entity!))

(define (remote-exec world procedure . rest)
  (make-response (apply (vector-ref execute-procedures procedure) (cons world rest))
                 (entity-ref world (first rest)) world))

(define (remote-execs world list)

  (define (exec-request request)
    (make-response
     (apply (vector-ref execute-procedures (first request)) (cons world (rest request)))
     (entity-ref world (second request))
     world))

  (string-append "(" (string-join (map exec-request (with-input-from-string list read))) ")"))

(define (remote-draw world)
  (define response '())
  (define (draw-entity symbol x y)
    (set! response (cons (list symbol x y) response)))
  (draw-entities world draw-entity)
  (list->string response))

(module+ test
  (require rackunit)

  (test-case
   "add response"
   (check-equal? (remote-add (make-world 3) type-bot 1 2) "(3 #t (101 0 (1 2) #f) ())"))

  (test-case
   "failed add response"
   (check-equal? (remote-add (make-world 3) type-bot -1 0) "(3 #f (0 0 (0 0) #f) ())"))
  )
