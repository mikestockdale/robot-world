#lang racket

(require web-server/servlet web-server/servlet-env)
(require "entity.rkt" "location.rkt" "world.rkt" "remote-world.rkt")

(define world (make-world 50))

(define (populate-world)
  (add-entity! world type-block (location 25 25))
  (add-entity! world type-block (location 35 25))
  (add-entity! world type-block (location 25 35))
  (add-entity! world type-block (location 45 25))
  (add-entity! world type-block (location 25 45))
  (add-entity! world type-block (location 15 25))
  (add-entity! world type-block (location 25 15))
  (add-entity! world type-block (location 5 25))
  (add-entity! world type-block (location 25 5))
  #t)

(define-values (dispatch url)
  (dispatch-rules
   (("add" (integer-arg) (integer-arg) (integer-arg)) add)
   (("move" (integer-arg) (integer-arg)) move)
   (("take" (integer-arg) (integer-arg)) take)
   (("drop" (integer-arg) (integer-arg)) drop)
   (("draw") draw)
   (else error)))

(define (add request type x y)
  (response/xexpr (remote-add world type x y)))

(define (drop request id direction)
  (response/xexpr (remote-drop world id direction)))

(define (move request id direction)
  (response/xexpr (remote-move world id direction)))

(define (take request bot-id block-id)
  (response/xexpr (remote-take world bot-id block-id)))
  
(define (draw request)
  (response/xexpr (remote-draw world)))

(define (error request) (response/xexpr "error"))

(define (start request) (dispatch request))

(populate-world)
(serve/servlet start
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:launch-browser? #f)
