#lang racket

(provide remote-add remote-draw remote-drop remote-move remote-take)
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

(define (remote-drop world id direction)
  (make-response (drop-entity! world id direction) (entity-ref world id) world))

(define (remote-move world id direction)
  (make-response (move-entity! world id direction) (entity-ref world id) world))

(define (remote-take world bot-id block-id)
  (make-response (take-entity! world bot-id block-id) (entity-ref world bot-id) world))

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
